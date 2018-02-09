{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: QuickCheck instances to help with testing
module GraphQL.Internal.Arbitrary
  ( arbitraryText
  , arbitraryNonEmpty
  ) where

import Protolude

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.String
import Test.QuickCheck (Gen, Arbitrary(..), arbitrary, listOf1)

-- | Generate arbitrary 'Text'.
arbitraryText :: Gen Text
arbitraryText = toS <$> arbitrary @Data.String.String

-- | Generate an arbitrary 'NonEmpty' list.
arbitraryNonEmpty :: forall a. Arbitrary a => Gen (NonEmpty a)
arbitraryNonEmpty =
  -- NonEmpty.fromList panics, but that's OK, because listOf1 is guaranteed to
  -- return a non-empty list, and because a panic in a test is highly
  -- informative and indicative of a bug.
  NonEmpty.fromList <$> listOf1 arbitrary

