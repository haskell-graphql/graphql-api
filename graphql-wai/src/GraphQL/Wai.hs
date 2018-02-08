{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic WAI handlers for graphql-api
module GraphQL.Wai
  ( toApplication
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import Network.Wai (Application, queryString, responseLBS)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status400)

import GraphQL (interpretAnonymousQuery)
import GraphQL.API (HasObjectDefinition)
import GraphQL.Resolver (HasResolver, Handler)
import GraphQL.Value (toValue)


-- | Adapt a GraphQL handler to a WAI application. This is really just
-- to illustrate the mechanism, and not production ready at this point
-- in time.
--
-- If you have a 'Cat' type and a corresponding 'catHandler' then you
-- can use "toApplication @Cat catHandler".
toApplication
  :: forall r. (HasResolver IO r, HasObjectDefinition r)
  => Handler IO r -> Application
toApplication handler = app
  where
    app req respond =
      case queryString req of
        [("query", Just query)] -> do
          r <- interpretAnonymousQuery @r handler (toS query)
          let json = Aeson.encode (toValue r)
          respond $ responseLBS status200 [(hContentType, "application/json")] json
        _ -> respond $ responseLBS status400 [] "Must provide excatly one query GET argument."
