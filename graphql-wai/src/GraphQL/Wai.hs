{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GraphQL.Wai
  ( toApplication
  ) where

import Protolude

import GraphQL (interpretAnonymousQuery)
import GraphQL.Resolver (HasResolver, Handler)
import Network.Wai (Application, queryString, responseLBS)
import GraphQL.Value.ToValue (toValue)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (status200, status400)
import qualified Data.Aeson as Aeson


-- | Adapt a GraphQL handler to a WAI application. This is really just
-- to illustrate the mechanism, and not production ready at this point
-- in time.
--
-- If you have a 'Cat' type and a corresponding 'catHandler' then you
-- can use "toApplication @Cat catHandler".
toApplication :: forall r. (HasResolver IO r) => Handler IO r -> Application
toApplication handler = app
  where
    app req respond =
      case queryString req of
        [("query", Just query)] -> do
          r <- interpretAnonymousQuery @r handler (toS query)
          let json = Aeson.encode (toValue r)
          respond $ responseLBS status200 [(hContentType, "application/json")] json
        _ -> respond $ responseLBS status400 [] "Must provide excatly one query GET argument."
