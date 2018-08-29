{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils
  ( json
  , shouldBeJSON
  ) where

import Protolude hiding (diff)

import Data.Aeson hiding (json)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Diff as Diff
import Data.Aeson.Pointer (formatPointer)
import qualified Data.ByteString.Lazy as LBS
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
  in liftIO $ unless (response' == js) $ do
    let changes = map presentOperation $ patchOperations $ diff js response'
    -- expectationFailure . show $ "Expected:\n" <> encode js <> "\nbut got:" <> encode response'
    expectationFailure . show $ "JSON values do not match: " <> T.intercalate ", " changes

presentOperation :: Diff.Operation -> Text
presentOperation (Add p v) = "add "     <> formatPointer p <> " = " <> encodeText v
presentOperation (Rep p v) = "replace " <> formatPointer p <> " = " <> encodeText v
presentOperation (Rem p)   = "remove "  <> formatPointer p
presentOperation (Cpy p f) = "copy "    <> formatPointer p <> "from " <> encodeText f
presentOperation (Mov p f) = "move "    <> formatPointer p <> "from " <> encodeText f
presentOperation (Tst p v) = "test "    <> formatPointer p <> " = "   <> encodeText v

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . LBS.toStrict . encode