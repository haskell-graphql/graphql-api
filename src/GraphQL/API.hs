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
import GHC.TypeLits (KnownSymbol, symbolVal)

import GraphQL.Input (CanonicalQuery)
import GraphQL.Output
  ( Response
  , ToValue(..)
  , Field(..)
  , singleton
  )


-- | A GraphQL application takes a canonical query and returns a response.
-- XXX: Really unclear what type this should be. Does it need IO? Generic
-- across Monad? Something analogous to the continuation-passing style of
-- WAI.Application? Can we make `HasGraph` parametrized on this?
type Application = CanonicalQuery -> IO Response


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
runQuery = resolve

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
