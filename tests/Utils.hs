{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils
  ( json
  , shouldBeJSON
  ) where

import Protolude

import Data.Aeson hiding (json)
import Data.Aeson.QQ (aesonQQ)
import Data.String (IsString(..))
import qualified Data.Text as T
import Language.Haskell.TH.Quote (QuasiQuoter )
import Test.Tasty.Hspec (expectationFailure)

import GraphQL.Internal.Name 

instance IsString (Maybe Name) where
  fromString = either (const Nothing) Just . makeName . T.pack

json :: QuasiQuoter
json = aesonQQ

infixl 1 `shouldBeJSON`

shouldBeJSON :: (ToJSON a, MonadIO m) => a -> Value -> m ()
shouldBeJSON response js = 
  let response' = toJSON response
  in liftIO $ unless (response' == js) $
    expectationFailure . show $ "Expected:\n" <> encode js <> "\nbut got:" <> encode response'