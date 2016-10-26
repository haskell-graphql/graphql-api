{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant-like GraphQL API.
--
-- This module will have all the things necessary to define and implement a
-- type-level API.
module GraphQL.API
  (
    (:>)
  , runQuery
  , Handler
  , Server
  , GraphQLValue
  ) where

import Protolude

import qualified Data.GraphQL.AST as AST
import qualified Data.List.NonEmpty as NonEmpty
import GHC.TypeLits (KnownSymbol, symbolVal)

import GraphQL.Input (CanonicalQuery)
import GraphQL.Output (Response(..), Error(..))
import GraphQL.Value
  ( ToValue(..)
  , Field(..)
  , Value(..)
  , singleton
  )

-- | A GraphQL resolver? handler? takes a canonical query and returns a response.
--
-- TODO: Pick a better name than 'Application' and rename to it.
type Application = CanonicalQuery -> IO Value

-- TODO: Add a "real" Application type that takes a GraphQL Document and
-- returns a Response. Probably can get away with it being generic across
-- monad, or even functor (because we don't need to do much with the result).
--
-- 'runApplication' would then:
--   - transform the document into a canonical query, returning
--     validation / syntax errors if it failed
--   - send the canonical query to the designated handler (really need to
--     settle on terminology)
--   - ensure the result of that is a valid response
--
-- NB: All of this is separate from the web, HTTP, JSON, etc.
--
-- We can then have another layer which embeds a GraphQL Application into WAI.
--
-- This is made much easier by GraphQL not supporting streaming.

-- | A field within an object.
--
-- e.g.
--  "foo" :> Foo
data (name :: k) :> a deriving (Typeable)

-- XXX: This structure is cargo-culted from Servant, even though jml doesn't fully
-- understand it yet.
--
-- XXX: Rename "Handler" to "Resolver"? Resolver is a technical term within
-- GraphQL, so only use it if it matches exactly.
type Handler = IO
-- XXX: Rename to Graph & GraphT?
type Server api = ServerT api Handler

class HasGraph api where
  type ServerT api (m :: * -> *) :: *
  resolve :: Proxy api -> Server api -> Application

data GraphQLValue (t :: *)

runQuery :: HasGraph api => Proxy api -> Server api -> CanonicalQuery -> IO Response
runQuery p s q = do
  result <- resolve p s q
  case result of
    ValueMap m -> pure (Success m)
    -- XXX: Can we push this check up to the type level? After all, the person
    -- who constructs the Server knows whether it returns a Map or something
    -- else.
    _ -> pure (ExecutionFailure (NonEmpty.fromList [Error "Top-level must return map." []]))


instance ToValue t => HasGraph (GraphQLValue t) where
  type ServerT (GraphQLValue t) m = m t

  resolve Proxy handler [] = toValue <$> handler
  resolve _ _ _ = empty

-- | A field within an object.
--
-- e.g.
--  "foo" :> Foo
instance (KnownSymbol name, HasGraph api) => HasGraph (name :> api) where
  type ServerT (name :> api) m = ServerT api m

  resolve Proxy subApi query =
    case lookup query fieldName of
      Nothing -> empty  -- XXX: Does this even work?
      Just (alias, subQuery) -> toValue <$> buildField alias (resolve (Proxy :: Proxy api) subApi subQuery)
    where
      fieldName = toS (symbolVal (Proxy :: Proxy name))
      -- XXX: What to do if there are argumentS?
      -- XXX: This is almost certainly partial.
      lookup q f = listToMaybe [ (a, s) | AST.SelectionField (AST.Field a n [] _ s) <- q
                                        , n == f
                                        ]
      buildField alias' value = do
        value' <- value
        -- XXX: An object? Really? jml thinks this should just be a key/value
        -- pair and that some other layer should assemble an object.
        pure (singleton (Field alias' value'))


-- TODO: Something with arguments

-- TODO: GraphQLObject as distinct from GraphQLValue. Will probably need a new
-- typeclass that allows getting fields by name. Might even be where we do the
-- argument handling. Might also let us move the map/object check in runQuery
-- to type level.
--
-- Notes from earlier:
-- XXX: I expect we can do something with typeclasses such that we can map
-- Haskell types to GraphQL types via a restricted set of constructors that
-- only allow building valid GraphQL objects and provide methods that allow
-- for the instances *here* to introspect them.
--
-- e.g.
--
-- class GraphQLObject a where
--   toGraphQLObject :: a -> GraphQLObject
--   getField :: Name -> a -> GraphQLObject

data a :<|> b = a :<|> b
infixr 8 :<|>

-- XXX: I wonder if this would be better as:
--
--   name1 :> api1 :<|> name2 :> api2
--
-- Then that might merging easier.
instance (HasGraph api1, HasGraph api2)
  => HasGraph (api1 :<|> api2) where
  type ServerT (api1 :<|> api2) m = ServerT api1 m :<|> ServerT api2 m

  resolve Proxy (api1 :<|> api2) query =
    merge <$> resolve (Proxy :: Proxy api1) api1 query
          <*> resolve (Proxy :: Proxy api2) api2 query

    where
      merge _ _ = notImplemented
