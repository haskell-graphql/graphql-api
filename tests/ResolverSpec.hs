{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ResolverSpec (spec) where

import Protolude hiding (Enum)

import Test.Hspec

import Data.Aeson (encode, toJSON, object, (.=), Value(Null))
import GraphQL
  ( Response(..)
  , interpretAnonymousQuery
  )
import GraphQL.API
  ( Object
  , Field
  , Argument
  , Enum
  , List
  , (:>)
  )
import GraphQL.Resolver
  ( Handler
  , ResolverError(..)
  , (:<>)(..)
  , returns
  , handlerError
  )
import GraphQL.Internal.Output (singleError)
import qualified GraphQL.Value as GValue
import EnumTests ( Mode(NormalFile) )

-- Test a custom error monad
type TMonad = ExceptT Text IO
type T = Object "T" '[] '[ Field "z" Int32
                         , Argument "x" Int32 :> Field "t" Int32
                         , Argument "y" Int32 :> Field "q" (Maybe Int32)
                         , Argument "d" Double :> Field "r" Double
                         , Field "l" (List Int32)
                         , Argument "n" Text :> Field "foo" (Maybe Foo)
                         , Field "bar" (Maybe Foo)
                         ]

tHandler :: Handler TMonad T
tHandler = pure $
  returns 10
  :<> (\x -> if x == 99 then handlerError "missed 99th value" else returns x)
  :<> returns . Just . (returns . (*2))
  :<> (\dArg -> if dArg == 9.9 then handlerError "bad 9.9 value" else returns dArg)
  :<> returns ([ returns 0, returns 7, handlerError "no number 9" ])
  :<> (\_nArg -> returns $ Just $ return $ returns "fred")
  :<> returns Nothing

-- https://github.com/jml/graphql-api/issues/119
-- Maybe X didn't descend into its argument. Now it does.
type Query = Object "Query" '[]
  '[ Argument "id" Text :> Field "test" (Maybe Foo) ]

type Foo = Object "Foo" '[]
  '[ Field "name" Text ]

data ServerFoo = ServerFoo
  { name :: Text
  } deriving (Eq, Show)

lookupFoo :: Text -> IO (Maybe ServerFoo)
lookupFoo _ = pure $ Just (ServerFoo "Mort")

viewFoo :: ServerFoo -> Handler IO Foo
viewFoo ServerFoo { name=name } = pure $ returns $ name

handler :: Handler IO Query
handler = pure $ \fooId -> do
  foo <- lookupFoo fooId
  returns $ viewFoo <$> foo

-- Enum test
type EnumQuery = Object "File" '[]
  '[ Field "mode" (Enum "modeEnumName" Mode) ]

enumHandler :: Handler IO EnumQuery
enumHandler = pure $ returns NormalFile

enumHandler2 :: Handler IO EnumQuery
enumHandler2 = pure $ handlerError "I forgot!"

-- /Enum test

spec :: Spec
spec = describe "TypeAPI" $ do
  describe "tTest" $ do
    it "works in a simple Int32 case" $ do
      Right (Success obj) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t(x: 12) }")
      encode obj `shouldBe` "{\"t\":12}"
    it "works in a simple Double case" $ do
      r <- runExceptT (interpretAnonymousQuery @T tHandler "{ r(d: 1.2) }")
      case r of
        Right (Success obj) -> encode obj `shouldBe` "{\"r\":1.2}"
        _ -> r `shouldNotBe` r
    it "works for value and error list elements" $ do
      r <- runExceptT (interpretAnonymousQuery @T tHandler "{ l }")
      case r of
        Right (PartialSuccess obj err) -> do
          encode obj `shouldBe` "{\"l\":[0,7,null]}"
          err `shouldBe` (singleError (HandlerError "no number 9"))
        _ -> r `shouldNotBe` r
    it "works for Nullable present elements" $ do
      r <- runExceptT (interpretAnonymousQuery @T tHandler "{ foo(n: \"flintstone\") { name } }")
      case r of
        Right (Success obj) -> do
          encode obj `shouldBe` "{\"foo\":{\"name\":\"fred\"}}"
        _ -> r `shouldNotBe` r
    it "works for Nullable null elements" $ do
      r <- runExceptT (interpretAnonymousQuery @T tHandler "{ bar { name } }")
      case r of
        Right (Success obj) -> do
          encode obj `shouldBe` "{\"bar\":null}"
        _ -> r `shouldNotBe` r
    it "complains about a missing field" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ not_a_field }")
      errs `shouldBe` singleError (FieldNotFoundError "not_a_field")
    it "complains about a handler throwing an exception" $ do
      r <- runExceptT (interpretAnonymousQuery @T tHandler "{ t(x: 99) }")
      case r of
        Right (PartialSuccess v errs) -> do
          -- n.b. this hasn't gone through the final JSON embedding,
          -- so it's the individual components instead of the final
          -- response of '{ "data": ..., "errors": ... }'
          errs `shouldBe` (singleError (HandlerError "missed 99th value"))
          toJSON (GValue.toValue v) `shouldBe` object [ "t" .= Null ]
        _ -> r `shouldNotBe` r
    it "complains about missing argument" $ do
      Right (PartialSuccess _ errs) <- runExceptT (interpretAnonymousQuery @T tHandler "{ t }")
      errs `shouldBe` singleError (ValueMissing "x")
  describe "issue 119" $ do
    it "Just works" $ do
      Success obj <- interpretAnonymousQuery @Query handler "{ test(id: \"10\") { name } }"
      encode obj `shouldBe` "{\"test\":{\"name\":\"Mort\"}}"
  describe "Parse, validate and execute queries against API" $ do
    it "API.Enum works" $ do
      Success obj <- interpretAnonymousQuery @EnumQuery enumHandler "{ mode }"
      encode obj `shouldBe` "{\"mode\":\"NormalFile\"}"
    it "API.Enum handles errors" $ do
      r <- interpretAnonymousQuery @EnumQuery enumHandler2 "{ mode }"
      case r of
        (PartialSuccess obj errs) -> do
          encode obj `shouldBe` "{\"mode\":null}"
          errs `shouldBe` (singleError $ HandlerError "I forgot!")
        _ -> r `shouldNotBe` r
