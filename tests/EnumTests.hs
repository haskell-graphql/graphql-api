{-# LANGUAGE DeriveGeneric #-}
module EnumTests ( Mode(Directory, NormalFile, ExecutableFile, Symlink) ) where

import Protolude hiding (Enum)

import GraphQL.API.Enum (GraphQLEnum)

-- https://github.com/jml/graphql-api/issues/116
-- Generic enum code is broken

data Mode = Directory | NormalFile | ExecutableFile | Symlink deriving (Show, Eq, Generic)

instance GraphQLEnum Mode
