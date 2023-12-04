# my-project

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT, runStderrLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Int (Int64)
-- import           Database.Persist
-- import           Database.Persist.Sql
-- import           Database.Persist.TH
-- import           Database.Persist (ToBackendKey, get)
-- import           Database.Persist.Postgresql (ConnectionString, Key, SqlBackend, runMigration, runSqlPersistMPool, withPostgresqlPool)
-- import           Database.PostgreSQL.Simple (SqlError)

import           Data.String
import           Data.UUID             (UUID)
import qualified Data.UUID             as UUID
import           Data.UUID.V4 (nextRandom)
import           Data.Time
import           Data.Text as T
import Data.Char
import Web.Scotty

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- PersonTest sql=PersonTest
--   person_id String
--   Primary person_id
--   first_name String
--   father_last_name String Maybe
--   mother_last_name String Maybe
--   birthday String Maybe
--   gender String Maybe
--   created_at UTCTime default=CURRENT_TIMESTAMP
--   updated_at UTCTime Maybe
--   deleted_at UTCTime Maybe
--   family_id FamilyTestId Maybe
--   deriving Show
-- FamilyTest sql=FamilyTest
--   family_id String
--   Primary family_id
--   father_id PersonTestId
--   mother_id PersonTestId
--   created_at UTCTime default=CURRENT_TIMESTAMP
--   updated_at UTCTime Maybe
--   deleted_at UTCTime Maybe
--   deriving Show
-- |]

connStr = "host=localhost dbname=example user=postgres password=postgres port=5432"

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    get "/api" $ do
      text "hello world!"
      -- liftIO $ print "someFunc amigo"
      -- html $ mconcat ["<h1>Scotty, me up!</h1>"]

-- main :: IO ()
-- main = runStderrLoggingT $
--   withPostgresqlPool connStr 10 $
--     \pool ->
--      liftIO $
--      do flip runSqlPersistMPool pool $
--           do
--             --  runMigration migrateAll
--             uuid <- liftIO nextRandom
--             time <- liftIO getCurrentTime
--             liftIO $ print "hola"
--             -- personId <- insert $ PersonTest (UUID.toString $ uuid) "A" (Just "B") (Just "C") Nothing Nothing time Nothing Nothing Nothing
--             -- liftIO $ print personId
--             -- TODO johnId <- ("b40b3669-3525-4fb3-a295-f790955f54br" :: String)
--             -- oneJohnPost <- selectList [PersonTestId ==. personId] [LimitTo 1]
--             -- liftIO $ print (oneJohnPost :: [Entity PersonTest])
--             return ()


--     -- Just user <- getUser 1
--     -- let user' = user { userEmail = "makenoise@example.com" }
--     -- newUserId <- insertUser user'
--     -- liftIO $ print newUserId



****************************************************************

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import GHC.Generics
import Web.Scotty
import Data.Time
import Data.Aeson (FromJSON, ToJSON)

getNow :: IO UTCTime
getNow = do
  now <- getCurrentTime
  pure now

data List = List { listId :: Int, name :: String, commentary :: String } deriving (Show, Generic)

instance ToJSON List
instance FromJSON List

-- data Todo = Todo { todoId :: Int, name :: String, commentary :: String } deriving (Show)

-- data List = List { listId :: Int, name :: String, commentary :: String, createdAt :: UTCTime, updatedAt :: UTCTime } deriving (Show)
-- data Todo = Todo { todoId :: Int, name :: String, commentary :: String } deriving (Show)

matchesId :: Int -> List -> Bool
matchesId id list = listId list == id

list1 :: List
list1 = List {
  listId = 1,
  name = "List 1",
  commentary = "Course de la semaine"
}

list2 :: List
list2 = List {
  listId = 2,
  name = "List 2",
  commentary = "Book to read"
}

allLists :: [List]
allLists = [list1, list2]

getLists :: ActionM ()
getLists = do
  -- text "hola abuelita Fanny!"
  json allLists

getList :: ActionM ()
getList = do
  id <- param "id"
  -- putStrLn formatTime defaultTimeLocale "%d.%m.%Y, %H:%M" createdAt1
  -- text ("Want to get the list id " <> id <> "!")
  json (filter (matchesId id) allLists)

routes :: ScottyM ()
routes = do
  get "/api/lists" getLists
  get "/api/lists/:id" getList

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 routes


********************************

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Database.PostgreSQL.Simple
import GHC.Generics
import Web.Scotty (delete, get, post, put, scotty, ActionM, json)
import Data.Aeson (FromJSON, ToJSON)

localPG :: ConnectInfo
localPG =
  defaultConnectInfo
    { connectHost = "localhost",
      connectDatabase = "example",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

data Product = Product
  { idProduct :: Int,
    name :: String,
    price :: Int,
    description :: String
  } deriving (Show, Generic)

instance ToJSON Product
instance FromJSON Product

product1 :: Product
product1 = Product {
  idProduct = 1,
  name = "Product name",
  price = 100,
  description = "Book to read"
}

allProducts :: [Product]
allProducts = [product1]

getProducts :: Connection -> ActionM ()
getProducts conn = do
  json allProducts

routes :: Connection -> IO ()
routes conn = scotty 8080 $ do
  get "/api/products" $ getProducts conn

  -- get "/api/products/:id" $ getProduct conn

  -- post "/api/products/" $ createProduct conn

  -- put "/api/products/:id" $ updateProduct conn

  -- delete "/api/products/:id" $ deleteProduct conn

main :: IO ()
main = do
  conn <- connect localPG
  routes conn


****************************************************************

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RecordWildCards   #-}

-- {-# OPTIONS_GHC -fwarn-unused-matches -fwarn-unused-binds -fwarn-unused-imports #-}


module Main (main) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
-- import Database.PostgreSQL.Simple
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT, NoLoggingT)
-- import           Control.Monad.Logger (NoLoggingT, runNoLoggingT, runStderrLoggingT)
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Postgresql (ConnectionString, Key, SqlBackend, runMigration, runSqlPersistMPool, withPostgresqlPool)
import GHC.Generics
import GHC.List
import Web.Scotty (html, delete, get, post, put, scotty, ActionM, json, text, middleware)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), (.:), (.=), pairs, object, withObject)
import Data.Text
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Data.Monoid ((<>))


-- localPG :: ConnectInfo
-- localPG =
--   defaultConnectInfo
--     { connectHost = "localhost",
--       connectDatabase = "example",
--       connectUser = "postgres",
--       connectPassword = "postgres"
--     }

-- Data type which describes the request which
-- will be received to create a product
data Product = Product
  { idProduct :: Int,
    name :: String,
    price :: Int,
    description :: String
  } deriving (Generic)

-- instance ToJSON Product
-- We define a FromJSON instance for Product
-- because we will want to parse it from a HTTP request
-- body (JSON).
-- instance FromJSON Product

product1 :: Product
product1 = Product {
  idProduct = 1,
  name = "Product name",
  price = 100,
  description = "Book to read"
}

allProducts :: [Product]
allProducts = [product1]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Products sql=products
  name Text
  description Text
  price Int
  deriving Show Eq Generic
|]

connStr = "host=localhost dbname=example user=postgres password=postgres port=5432"

inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    -- runMigration migrateAll
    action

-- getProducts :: ActionM ()
-- getProducts = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
--      liftIO $ flip runSqlPersistMPool pool $ do
--         -- runMigration migrateAll
--         -- liftIO $ print "hola abuelita Fanny"
--         -- json allProducts
--         res  :: [Entity Products] <- selectList [] [LimitTo 1]
--         liftIO $ print res

-- -- getProducts = do
--   -- json allProducts

-- getProducts :: ActionM -> IO a
-- getProducts = inBackend $ do
--   res  :: [Entity Products] <- selectList [] [LimitTo 1]
--   -- liftIO $ print res
--   json (res :: [Entity Products])

getProducts :: ActionM ()
getProducts = do
  -- inBackend $ do
  --   json allProducts
  -- json [(0::Int)..10]
  -- liftIO $ do
  --   res  :: [Entity Products] <- selectList [] [LimitTo 1]
  --   liftIO $ json (res :: [Entity Products])
  -- res <- liftIO $ do
  --   inBackend $ do
  --     res  :: [Entity Products] <- selectList [] [LimitTo 1]
  --     -- liftIO $ print res
  --     return res
  --     -- liftIO $ json (res :: [Entity Products])
  -- res <- (liftIO $ inBackend $ do selectList [] [LimitTo 1]) :: ActionM [Products]
  -- json [(0::Int)..5]
  -- ts <- liftIO $ inBackend (selectList [] [LimitTo 1])
  -- json (ts :: [Entity Products])
  -- res <- inBackend $ (selectList [] [LimitTo 1])
  json [(0::Int)..5]


routes :: IO ()
routes = scotty 8080 $ do
  Web.Scotty.get "/api/products" getProducts
    -- json [(0::Int)..10]
    -- json ("hello world" :: String)
    -- text "Success"
    -- liftIO $ inBackend $ json allProducts
      -- res  :: [Entity Products] <- selectList [] [LimitTo 1]
      -- json (res :: [Entity Products])
      -- liftIO $ json allProducts

--   -- get "/api/products/:id" $ getProduct conn

--   -- post "/api/products/" $ createProduct conn

--   -- put "/api/products/:id" $ updateProduct conn

--   -- delete "/api/products/:id" $ deleteProduct conn

-- main :: IO ()
-- main = do
--   routes

-- main :: IO ()
-- main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
--      liftIO $ flip runSqlPersistMPool pool $ do
--         -- printMigration migrateAll
--         res  :: [Entity Products] <- selectList [] [LimitTo 1]
--         -- res :: [Entity Products] <- selectList [ProductsId ==. 1] [LimitTo 1]
--         liftIO $ print res

-- main :: IO ()
-- main = inBackend $ do
--   res  :: [Entity Products] <- selectList [] [LimitTo 1]
--   liftIO $ print res

-- main :: IO ()
-- main = do
--     dbFunction doDbStuff
--     scotty 3000 $ do
--         Web.Scotty.middleware logStdoutDev
--         Web.Scotty.get "/api/products" $ json [(0::Int)..10]

-- instance ToJSON (Entity Products) where
--   toJSON (Entity uid (c@Products{..})) =
--     object
--     [ "id" .= uid
--     , "name" .= productsName
--     , "price" .= productsPrice
--     , "description" .= productsDescription
--     ]

-- instance FromJSON Products where
--     parseJSON (Object v) =
--         Products <$> v .: "name"
--                  <*> v .: "price"
--                  <*> v .: "description"
--     parseJSON _ = mzero

instance ToJSON (Entity Products) where
    toJSON (Entity uid (c@Products{..})) =
      object
      [ "id" .= uid
      , "name" .= productsName
      , "price" .= productsPrice
      , "description" .= productsDescription
      ]

-- instance FromJSON Products where
--     parseJSON = withObject "Products" $ \v -> Products
--         <$> v .: "name"
--         <*> v .: "price"
--         <*> v .: "description"

-- instance ToJSON (Entity User) where
--     toJSON = keyValueEntityToJSON
-- @
-- keyValueEntityToJSON :: (PersistEntity Products, ToJSON Products, ToJSON (Key Products))
--                      => [Entity Products] -> Value
-- keyValueEntityToJSON (Products id name price) = object
--     [ "id" .= id
--     , "name" .= name
--     , "price" .= price
-- ]

main :: IO ()
main = do
  scotty 3000 $ do
    Web.Scotty.middleware logStdoutDev
    Web.Scotty.get "/api/products" $ do
      (products :: [Entity Products]) <-
        liftIO $ inBackend $ selectList [] []
      json products
      -- json $ GHC.List.take 2 $ [(0::Int)..10]
      -- json $ GHC.List.length products

-- inAppDb = liftIO $ dbFunction doDbStuff

-- dbFunction query = runStderrLoggingT $
--         withPostgresqlPool connStr 10 $
--         \pool -> liftIO $ runSqlPersistMPool query pool

-- doDbStuff = do
--   res  :: [Entity Products] <- selectList [] [LimitTo 1]
--   liftIO $ print "hola abuelita Fanny"
--   liftIO $ print res
