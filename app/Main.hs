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
{-# LANGUAGE RecordWildCards            #-}

module Main (main) where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT, NoLoggingT)
import Database.Persist
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Database.Persist.Postgresql (ConnectionString, SqlBackend, runMigration, runSqlPersistMPool, withPostgresqlPool)
import GHC.Generics
import Web.Scotty (html, delete, get, post, put, scotty, ActionM, json, text, middleware)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.=), pairs, object, withObject)
import Data.Text
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Products sql=products
  name Text
  description Text
  price Int
  deriving Show Eq Generic
|]

connStr :: Database.Persist.Postgresql.ConnectionString
connStr = "host=localhost dbname=example user=postgres password=postgres port=5432"

inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend action = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    -- runMigration migrateAll
    action

instance ToJSON (Entity Products) where
  toJSON (Entity productId (c@Products{..})) =
    object
    [ "id" .= productId
    , "name" .= productsName
    , "price" .= productsPrice
    , "description" .= productsDescription
    ]

main :: IO ()
main = do
  scotty 3000 $ do
    Web.Scotty.middleware logStdoutDev
    Web.Scotty.get "/api/products" $ do
      (products :: [Entity Products]) <-
        liftIO $ inBackend $ selectList [] []
      json products
