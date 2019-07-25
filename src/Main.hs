{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, NamedFieldPuns, LambdaCase, OverloadedStrings #-}
module Main where

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


dbCredentials :: PGS.ConnectInfo
dbCredentials = ConnectInfo
  { connectHost = "localhost"
  , connectPort = (fromIntegral 5432)
  , connectUser = "hedgehog"
  , connectPassword = "hedgehog"
  , connectDatabase = "hedgehog"
  }

data Env = Env
  { envDbConnection :: Connection
  }

newtype TestM a = TestM (ReaderT Env IO a) deriving (MonadIO, Monad, Functor, Applicative, MonadMask, MonadThrow, MonadUnliftIO, MonadCatch, MonadReader Env)

class (MonadIO m) => HasDatabase m where
  getDbConnection :: m Connection

instance HasDatabase TestM where
  getDbConnection = envDbConnection <$> ask

runTestM :: Env -> TestM a -> IO a
runTestM env (TestM action) = runReaderT action env

withPool :: (Pool Connection -> IO ()) -> IO ()
withPool action = do
  pool <- Pool.createPool
    (PGS.connect dbCredentials) -- how to connect
    PGS.close                   -- how to close
    (fromIntegral 10)           -- how many seconds to keep inactive connections
    1                           -- number of strips
    10                          -- number of connections per stripe
  C.finally (action pool) (Pool.destroyAllResources pool)


withRollback :: Pool Connection -> TestM a -> IO a
withRollback pool action = Pool.withResource pool $ \conn ->
    let env = Env { envDbConnection = conn }
    in E.mask $ \restore -> C.finally
                            (do
                                liftIO $ PGS.begin conn
                                restore $ runTestM env action
                            )
                            (liftIO $ PGS.rollback conn)


data UserPoly a = UserPoly
  { userId :: a
  , userName :: Text
  , userEmail :: Text
  } deriving (Eq, Show)

type NewUser = UserPoly ()
type User = UserPoly Int

instance FromRow User where
  fromRow = UserPoly <$> PGS.field <*> PGS.field <*> PGS.field

createUser :: (HasDatabase m) => NewUser -> m User
createUser UserPoly{userName, userEmail} = do
  conn <- getDbConnection
  (liftIO $ PGS.query conn "insert into users(name, email) values(?, ?) returning id, name, email" (userName, userEmail)) >>= \case
    [] -> Prelude.error "Not exepecting insert...returning to result in an empty list"
    [r] -> pure r
    x -> Prelude.error $ "Not exepcting insert...returning to result in multiple rows: " <> show x

data Post = Post
  { postUserId :: Int
  , postTitle :: Text
  , postBody :: Text
  } deriving (Eq, Show)

newtype PostEntity = PostEntity (Int, Post) deriving (Eq, Show)

instance FromRow PostEntity where
  fromRow = PostEntity <$> ((,) <$> PGS.field <*> (Post <$> PGS.field <*> PGS.field <*> PGS.field))


createPost :: (HasDatabase m) => Post -> m PostEntity
createPost Post{postUserId, postTitle, postBody} = do
  conn <- getDbConnection
  (liftIO $ PGS.query conn "insert into posts(title, body) values(?, ?) returning id, user_id, post, title" (postUserId, postTitle, postBody)) >>= \case
    [] -> Prelude.error "Not exepecting insert...returning to result in an empty list"
    [r] -> pure r
    x -> Prelude.error $ "Not exepcting insert...returning to result in multiple rows: " <> show x


randomUser :: (MonadGen m) => m NewUser
randomUser = UserPoly <$> (pure ()) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll)

randomPost :: (MonadGen m) => User -> m Post
randomPost UserPoly{userId} = Post <$> (pure userId) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll) <*> (Gen.text (Range.constant 1 100) Gen.unicodeAll)


myProperty :: Pool Connection -> TestTree
myProperty pool = testProperty "My property" $ property $ withRollback pool $ do
  newuser <- forAll randomUser
  user <- createUser newuser
  newpost <- forAll $ randomgPost user
  post <- createPost newpost
  True === True

main :: IO ()
main = withPool $ \pool -> defaultMain $ testGroup "All tests" [myProperty pool]

