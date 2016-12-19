-- | GraphQL inputs.
--
-- Responsible for turning AST into a normal, fully-realized form.
module GraphQL.Internal.Input
  ( CanonicalQuery
  ) where

import qualified Data.GraphQL.AST as AST

-- | A query that has all its fragments, variables, and directives evaluated,
-- so that all that is left is a query with literal values.
--
-- 'SelectionSet' is maybe the closest type, but isn't quite what we want, as
-- it still has places for directives and other symbolic values.
type CanonicalQuery = AST.SelectionSet
