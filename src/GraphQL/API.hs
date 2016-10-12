{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for graphql-haskell.
module GraphQL.API
  ( API
  , server
  ) where

import Protolude hiding (Handler)

import Control.Monad.Log (MonadLog, Severity(..), WithSeverity)
import Data.Aeson (FromJSON, ToJSON)
import qualified NeatInterpolation as NI
import Servant
       ((:>), (:<|>)(..), Get, JSON, MimeRender(..), Raw, Server)

import GraphQL.ContentTypes (HTML)
import GraphQL.Instrument (metrics)
import qualified GraphQL.Logging as Log

data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

-- | graphql-haskell API definition.
type API = Get '[HTML] RootPage :<|> "users" :> Get '[JSON] [User] :<|> "metrics" :> Raw

-- | graphql-haskell API implementation.
server :: Server API
server = pure RootPage :<|> Log.withLogging users :<|> metrics

users
  :: (MonadIO m, MonadLog (WithSeverity LText) m)
  => m [User]
users = do
  Log.log Informational ("Example of logging" :: LText)
  pure [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]

-- | Represents the root page of the service.
data RootPage =
  RootPage

-- | Very simple root HTML page.
instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>graphql-haskell</title></head>
         <body>
         <h1>graphql-haskell</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/jml/graphql-haskell">https://github.com/jml/graphql-haskell/</a>
         </p>
         </body>
         <html>
         |]
