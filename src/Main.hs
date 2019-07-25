{-# LANGUAGE ApplicativeDo, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, LambdaCase, OverloadedStrings #-}
module Main where

import Control.Monad.Morph (hoist)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Control.Monad.Catch
import Control.Monad.Reader
import UnliftIO
import Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.FromRow as PGS
import Database.PostgreSQL.Simple.ToRow as PGS
import Data.Pool as Pool
import Control.Monad.Catch as C
import Control.Exception.Lifted as E
import Data.Text


--
-- First, create a Postgres user and DB locally:
--
-- $ createuser -d -l -P hedgehog
-- $ createdb -U hedgehog -W hedgehog
--


-- | Hard-coded DB credentials. If you've setup your local DB differently, then
-- you will have to change these
dbCredentials :: PGS.ConnectInfo
dbCredentials = ConnectInfo
  { connectHost = "localhost"
  , connectPort = (fromIntegral 5432)
  , connectUser = "hedgehog"
  , connectPassword = "hedgehog"
  , connectDatabase = "hedgehog"
  }


-- | Before attemptimg to run the tests, you will need to run this function in a
-- GHCi session
createTables :: IO ()
createTables = do
  conn <- PGS.connect dbCredentials
  C.finally
    (void $ PGS.execute_ conn $
      "create table users(id serial primary key, name text not null, email text not null);" <>
      "create table posts(id serial primary key, user_id integer not null references users(id), title text not null, body text not null);")
    (PGS.close conn)


-- | The app's environment consists of a DB connection, NOT the entire DB pool.
-- All the DB operations that the app does, need to happen through this single
-- DB connection.
data Env = Env
  { envDbConnection :: Connection
  }


-- | All the app code is polymorphic in `m` with a bunch of typeclass
-- constraints. This is one of the important constraints.
class (MonadIO m) => HasDatabase m where
  getDbConnection :: m Connection



-- | The app monad, which is called TestM in this case, but it will implement
-- the HasDatabase type-class below
newtype TestM a = TestM (ReaderT Env IO a)
  deriving (MonadIO, Monad, Functor, Applicative, MonadMask, MonadThrow, MonadUnliftIO, MonadCatch, MonadReader Env)


-- | HasDatabase implementation for TestM
instance HasDatabase TestM where
  getDbConnection = envDbConnection <$> ask

-- | Helper to unwrap the `TestM` newtype and run the ReaderT action embedded
-- inside it. I prefer flipping the order of arguments around.
runTestM :: Env -> TestM a -> IO a
runTestM env (TestM action) = runReaderT action env


-- | Helper function to create a top-level DB pool so that properties can be
-- tested in parallel. Each run/invocation of the property _should_ end-up
-- getting a different connection from this pool, so that they are isolated from
-- each other.
withPool :: (Pool Connection -> IO ()) -> IO ()
withPool action = do
  pool <- Pool.createPool
    (PGS.connect dbCredentials) -- how to connect
    PGS.close                   -- how to close
    (fromIntegral 10)           -- how many seconds to keep inactive connections
    1                           -- number of strips
    10                          -- number of connections per stripe
  C.finally (action pool) (Pool.destroyAllResources pool)

-- | Very important function, which prepares the environment for a `TestM`
-- action (basically takes a new connection from the DB pool), runs the action
-- with that DB connectiom, and ROLLS IT BACK, so that the DB is left in a
-- pristine state for subsequent tests/properties.
withRollback :: Pool Connection -> TestM a -> IO a
withRollback pool action = Pool.withResource pool $ \conn ->
    let env = Env { envDbConnection = conn }
    in E.mask $ \restore -> C.finally
                            (do
                                liftIO $ PGS.begin conn
                                restore $ runTestM env action
                            )
                            (liftIO $ PGS.rollback conn)

-- | A polymorphic record to represent a User. This is the style that Opaleye
-- (or Beam) uses.
data UserPoly a = UserPoly
  { userId :: a
  , userName :: Text
  , userEmail :: Text
  } deriving (Eq, Show)

-- | A new user where the `id` (primary-key) has no value
type NewUser = UserPoly ()

-- | A user saved to the DB, where the `id` is an Int provided by the DB itself.
type User = UserPoly Int

-- | PG-simple boilerplate
instance FromRow User where
  fromRow = UserPoly <$> PGS.field <*> PGS.field <*> PGS.field

-- | Function to create a user in any monad that implements the HasDatabase
-- type-class. Most of the app code is written in this style.
createUser :: (HasDatabase m) => NewUser -> m User
createUser UserPoly{userName, userEmail} = do
  conn <- getDbConnection
  (liftIO $ PGS.query conn "insert into users(name, email) values(?, ?) returning id, name, email" (userName, userEmail)) >>= \case
    [] -> Prelude.error "Not exepecting insert...returning to result in an empty list"
    [r] -> pure r
    x -> Prelude.error $ "Not exepcting insert...returning to result in multiple rows: " <> show x


-- | Post created by a user. Here I am using the Persistent way of doing things,
-- where the id/primary-key is kep separate from the core value. This is just
-- for demonstration; my codebase uses the Opaleye/Beam style demonstrated with
-- the `User` type above. The `Post` type represents an unsaved post.
--
-- Also, please note, that even to create an UNSAVED post, one needs the ID of a
-- SAVED user, due to FK constraints.
data Post = Post
  { postUserId :: Int
  , postTitle :: Text
  , postBody :: Text
  } deriving (Eq, Show)

-- | A newtype which represents a Post that has been saved to the DB. It
-- basically ties-together the id/primary-key along with the Post value.
newtype PostEntity = PostEntity (Int, Post) deriving (Eq, Show)

-- | Pg-simple boilerplate
instance FromRow PostEntity where
  fromRow = PostEntity <$> ((,) <$> PGS.field <*> (Post <$> PGS.field <*> PGS.field <*> PGS.field))

-- | Function to create a post in any monad that implements the HasDatabase
-- type-class. Most of the app code is written in this style.
createPost :: (HasDatabase m) => Post -> m PostEntity
createPost Post{postUserId, postTitle, postBody} = do
  conn <- getDbConnection
  (liftIO $ PGS.query conn "insert into posts(title, body) values(?, ?) returning id, user_id, post, title" (postUserId, postTitle, postBody)) >>= \case
    [] -> Prelude.error "Not exepecting insert...returning to result in an empty list"
    [r] -> pure r
    x -> Prelude.error $ "Not exepcting insert...returning to result in multiple rows: " <> show x


-- | Function to generate a random UNSAVED user.
randomUser :: (MonadGen m) => m NewUser
randomUser = UserPoly <$> (pure ()) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll)

newtype CreatePost = CreatePost { unCreatePost :: User -> Post }

instance Show CreatePost where
  showsPrec p x =
    showsPrec p (unCreatePost x (UserPoly 0 "" ""))

-- | Function to generate a randomg UNSAVED post. However, please note, that
-- this requires a SAVED User to be passed-in because `postUserId` is a
-- required, non-nullable FK field.
randomPost :: (MonadGen m) => m CreatePost
randomPost = do
  title <- Gen.text (Range.constant 1 100) Gen.unicodeAll
  body <- Gen.text (Range.constant 1 100) Gen.unicodeAll
  -- note you don't actually need the whole user here,
  -- maybe having a UserId newtype would be better
  pure . CreatePost $ \UserPoly{userId} ->
    Post userId title body

-- | This is the kind of test I am trying to write, but I have commented it out,
-- because it is not compiling. The uncommented code below is just boilerplate
-- to get the entire file to compile.
--
myTestTree :: Pool Connection -> TestTree
myTestTree pool =
  testProperty "My property" $ myProperty pool

myProperty :: Pool Connection -> Property
myProperty pool =
  property . hoist (withRollback pool) $ do
   newuser <- forAll randomUser
   newpost <- forAll randomPost
   -- evalM is a bit like a specialized 'try' or 'handle', will cause any
   -- exception thrown in the thing it is evaluating to be to be located to the
   -- 'evalM' line rather than just the property as a whole.
   user <- evalM . lift $ createUser newuser
   post <- evalM . lift $ createPost (unCreatePost newpost user)
   True === True
--myProperty :: Pool Connection -> TestTree
--myProperty pool = testProperty "My property" $ property $ pure ()


-- | main-related boilerplate.
main :: IO ()
main = withPool $ \pool -> defaultMain $ testGroup "All tests" [myTestTree pool]
