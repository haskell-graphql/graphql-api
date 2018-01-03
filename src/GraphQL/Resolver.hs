{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-} -- nicer type errors in some cases
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- for TypeError

module GraphQL.Resolver
  ( ResolverError(..)
  , HasResolver(..)
  , (:<>)(..)
  , Defaultable(..)
  , Result(..)
  , unionValue
  ) where

-- TODO (probably incomplete, the spec is large)
-- - input objects - I'm not super clear from the spec on how
--   they differ from normal objects.
-- - "extend type X" is used in examples in the spec but it's not
--   explained anywhere?
-- - Directives (https://facebook.github.io/graphql/#sec-Type-System.Directives)
-- - Enforce non-empty lists (might only be doable via value-level validation)

import Protolude hiding (Enum, TypeError)

import qualified Data.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage(..), Symbol, symbolVal)
import GHC.Types (Type)
import qualified GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import GraphQL.API
  ( (:>)
  , HasAnnotatedType(..)
  , HasAnnotatedInputType(..)
  )
import qualified GraphQL.API as API
import qualified GraphQL.Value as GValue
import GraphQL.Value
  ( Value
  , pattern ValueEnum
  )
import GraphQL.Value.FromValue (FromValue(..))
import GraphQL.Value.ToValue (ToValue(..))
import GraphQL.Internal.Name (Name, NameError(..), HasName(..), nameFromSymbol)
import qualified GraphQL.Internal.OrderedMap as OrderedMap
import GraphQL.Internal.Output (GraphQLError(..))
import GraphQL.Internal.Validation
  ( SelectionSetByType
  , SelectionSet(..)
  , Field
  , ValidationErrors
  , getSubSelectionSet
  , getSelectionSetForType
  , lookupArgument
  )

data ResolverError
  -- | There was a problem in the schema. Server-side problem.
  = SchemaError NameError
  -- | Couldn't find the requested field in the object. A client-side problem.
  | FieldNotFoundError Name
  -- | No value provided for name, and no default specified. Client-side problem.
  | ValueMissing Name
  -- | Could not translate value into Haskell. Probably a client-side problem.
  | InvalidValue Name Text
  -- | Found validation errors when we tried to merge fields.
  | ValidationError ValidationErrors
  -- | Tried to get subselection of leaf field.
  | SubSelectionOnLeaf (SelectionSetByType Value)
  -- | Tried to treat an object as a leaf.
  | MissingSelectionSet
  deriving (Show, Eq)

instance GraphQLError ResolverError where
  formatError (SchemaError e) =
    "Schema error: " <> formatError e
  formatError (FieldNotFoundError field) =
    "Field not supported by the API: " <> show field
  formatError (ValueMissing name) =
    "No value provided for " <> show name <> ", and no default specified."
  formatError (InvalidValue name text) =
    "Could not coerce " <> show name <> " to valid value: " <> text
  formatError (ValidationError errs) =
    "Validation errors: " <> Text.intercalate ", " (map formatError (NonEmpty.toList errs))
  formatError (SubSelectionOnLeaf ss) =
    "Tried to get values within leaf field: " <> show ss
  formatError MissingSelectionSet =
    "Tried to treat object as if it were leaf field."

-- | Object field separation operator.
--
-- Use this to provide handlers for fields of an object.
--
-- Say you had the following GraphQL type with \"foo\" and \"bar\" fields,
-- e.g.
--
-- @
--   type MyObject {
--     foo: Int!
--     bar: String!
--   }
-- @
--
-- You could provide handlers for it like this:
--
-- >>> :m +System.Environment
-- >>> let fooHandler = pure 42
-- >>> let barHandler = System.Environment.getProgName
-- >>> let myObjectHandler = pure $ fooHandler :<> barHandler :<> ()
data a :<> b = a :<> b
infixr 8 :<>


-- Result collects errors and values at the same time unless a handler
-- tells us to bail out in which case we stop the processing
-- immediately.
data Result a = Result [ResolverError] a deriving (Show, Functor, Eq)

-- Aggregating results keeps all errors and creates a ValueList
-- containing the individual values.
aggregateResults :: [Result Value] -> Result Value
aggregateResults r = toValue <$> sequenceA r

throwE :: Applicative f => ResolverError -> f (Result Value)
throwE err = pure (Result [err] GValue.ValueNull)

instance Applicative Result where
  pure v = Result [] v
  (Result e1 f) <*> (Result e2 x) = Result (e1 <> e2) (f x)

ok :: Value -> Result Value
ok = pure

class HasResolver m a where
  type Handler m a
  resolve :: Handler m a -> Maybe (SelectionSetByType Value) -> m (Result Value)

-- | Specify a default value for a type in a GraphQL schema.
--
-- GraphQL schema can have default values in certain places. For example,
-- arguments to fields can have default values. Because we cannot lift
-- arbitrary values to the type level, we need some way of getting at those
-- values. This typeclass provides the means.
--
-- To specify a default, implement this typeclass.
--
-- The default implementation is to say that there *is* no default for this
-- type.
class Defaultable a where
  -- | defaultFor returns the value to be used when no value has been given.
  defaultFor :: Name -> Maybe a
  defaultFor _ = empty

-- | Called when the schema expects an input argument @name@ of type @a@ but
-- @name@ has not been provided.
valueMissing :: Defaultable a => Name -> Either ResolverError a
valueMissing name = maybe (Left (ValueMissing name)) Right (defaultFor name)

instance Defaultable Int32

instance Defaultable Double

instance Defaultable Bool

instance Defaultable Text

instance Defaultable (Maybe a) where
  -- | The default for @Maybe a@ is @Nothing@.
  defaultFor _ = pure Nothing

instance forall m. (Applicative m) => HasResolver m Int32 where
  type Handler m Int32 = m Int32
  resolve handler Nothing = map (ok . toValue) handler
  resolve _ (Just ss) = throwE (SubSelectionOnLeaf ss)

instance forall m. (Applicative m) => HasResolver m Double where
  type Handler m Double = m Double
  resolve handler Nothing =  map (ok . toValue) handler
  resolve _ (Just ss) = throwE (SubSelectionOnLeaf ss)

instance forall m. (Applicative m) => HasResolver m Text where
  type Handler m Text = m Text
  resolve handler Nothing =  map (ok . toValue) handler
  resolve _ (Just ss) = throwE (SubSelectionOnLeaf ss)

instance forall m. (Applicative m) => HasResolver m Bool where
  type Handler m Bool = m Bool
  resolve handler Nothing =  map (ok . toValue) handler
  resolve _ (Just ss) = throwE (SubSelectionOnLeaf ss)

instance forall m hg. (Monad m, Applicative m, HasResolver m hg) => HasResolver m (API.List hg) where
  type Handler m (API.List hg) = m [Handler m hg]
  resolve handler selectionSet = do
    h <- handler
    let a = traverse (flip (resolve @m @hg) selectionSet) h
    map aggregateResults a

instance forall m ksN enum. (Applicative m, API.GraphQLEnum enum) => HasResolver m (API.Enum ksN enum) where
  type Handler m (API.Enum ksN enum) = m enum
  resolve handler Nothing = map (ok . GValue.ValueEnum . API.enumToValue) handler
  resolve _ (Just ss) = throwE (SubSelectionOnLeaf ss)

instance forall m hg. (HasResolver m hg, Monad m) => HasResolver m (Maybe hg) where
  type Handler m (Maybe hg) = m (Maybe (Handler m hg))
  resolve handler selectionSet = do
    result <- handler
    case result of
      Just x -> resolve @m @hg (x :: Handler m hg) selectionSet
      Nothing -> (pure . ok) GValue.ValueNull

-- TODO: A parametrized `Result` is really not a good way to handle the
-- "result" for resolveField, but not sure what to use either. Tom liked the
-- tuple we had before more because it didn't imply any other structure or
-- meaning. Maybe we can just create a new datatype. jml thinks we should
-- extract some helpful generic monad, ala `Validator`.
-- <https://github.com/jml/graphql-api/issues/98>
type ResolveFieldResult = Result (Maybe GValue.Value)

-- Extract field name from an argument type. TODO: ideally we'd run
-- this directly on the "a :> b" argument structure, but that requires
-- passing in the plain argument structure type into resolveField or
-- resolving "name" in the buildFieldResolver. Both options duplicate
-- code somwehere else.
type family FieldName (a :: Type) = (r :: Symbol) where
  FieldName (JustHandler (API.Field name t)) = name
  FieldName (PlainArgument a f) = FieldName f
  FieldName (EnumArgument a f) = FieldName f
  FieldName x = TypeError ('Text "Unexpected branch in FieldName type family. Please file a bug!" ':<>: 'ShowType x)

resolveField :: forall dispatchType (m :: Type -> Type).
  (BuildFieldResolver m dispatchType, Monad m, KnownSymbol (FieldName dispatchType))
  => FieldHandler m dispatchType -> m ResolveFieldResult -> Field Value -> m ResolveFieldResult
resolveField handler nextHandler field =
  -- check name before
  case nameFromSymbol @(FieldName dispatchType) of
    Left err -> pure (Result [SchemaError err] (Just GValue.ValueNull))
    Right name'
      | getName field == name' ->
          case buildFieldResolver @m @dispatchType handler field of
            Left err -> pure (Result [err] (Just GValue.ValueNull))
            Right resolver -> do
              Result errs value <- resolver
              pure (Result errs (Just value))
      | otherwise -> nextHandler

-- We're using our usual trick of rewriting a type in a closed type
-- family to emulate a closed typeclass. The following are the
-- universe of "allowed" class instances for field types:
data JustHandler a
data EnumArgument a b
data PlainArgument a b

-- injective helps with errors sometimes
type family FieldResolverDispatchType (a :: Type) = (r :: Type) | r -> a where
  FieldResolverDispatchType (API.Field ksA t) = JustHandler (API.Field ksA t)
  FieldResolverDispatchType (API.Argument ksB (API.Enum name t) :> f) = EnumArgument (API.Argument ksB (API.Enum name t)) (FieldResolverDispatchType f)
  FieldResolverDispatchType (API.Argument ksC t :> f) = PlainArgument (API.Argument ksC t) (FieldResolverDispatchType f)

-- | Derive the handler type from the Field/Argument type in a closed
-- type family: We don't want anyone else to extend this ever.
type family FieldHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  FieldHandler m (JustHandler (API.Field ksD t)) = Handler m t
  FieldHandler m (PlainArgument (API.Argument ksE t) f) = t -> FieldHandler m f
  FieldHandler m (EnumArgument (API.Argument ksF (API.Enum name t)) f) = t -> FieldHandler m f

class BuildFieldResolver m fieldResolverType where
  buildFieldResolver :: FieldHandler m fieldResolverType -> Field Value -> Either ResolverError (m (Result Value))

instance forall ksG t m.
  ( KnownSymbol ksG, HasResolver m t, HasAnnotatedType t, Monad m
  ) => BuildFieldResolver m (JustHandler (API.Field ksG t)) where
  buildFieldResolver handler field = do
    pure (resolve @m @t handler (getSubSelectionSet field))

instance forall ksH t f m.
  ( KnownSymbol ksH
  , BuildFieldResolver m f
  , FromValue t
  , Defaultable t
  , HasAnnotatedInputType t
  , Monad m
  ) => BuildFieldResolver m (PlainArgument (API.Argument ksH t) f) where
  buildFieldResolver handler field = do
    argument <- first SchemaError (API.getArgumentDefinition @(API.Argument ksH t))
    let argName = getName argument
    value <- case lookupArgument field argName of
      Nothing -> valueMissing @t argName
      Just v -> first (InvalidValue argName) (fromValue @t v)
    buildFieldResolver @m @f (handler value) field

instance forall ksK t f m name.
  ( KnownSymbol ksK
  , BuildFieldResolver m f
  , KnownSymbol name
  , Defaultable t
  , API.GraphQLEnum t
  , Monad m
  ) => BuildFieldResolver m (EnumArgument (API.Argument ksK (API.Enum name t)) f) where
  buildFieldResolver handler field = do
    argName <- first SchemaError (nameFromSymbol @ksK)
    value <- case lookupArgument field argName of
      Nothing -> valueMissing @t argName
      Just (ValueEnum enum) -> first (InvalidValue argName) (API.enumFromValue @t enum)
      Just value -> Left (InvalidValue argName (show value <> " not an enum: " <> show (API.enumValues @t)))
    buildFieldResolver @m @f (handler value) field

-- Note that we enumerate all ks variables with capital letters so we
-- can figure out error messages like the following that don't come
-- with line numbers:
--
--        • No instance for (GHC.TypeLits.KnownSymbol ks0)
--            arising from a use of ‘interpretAnonymousQuery’

-- We only allow Field and Argument :> Field combinations:
type family RunFieldsType (m :: Type -> Type) (a :: [Type]) = (r :: Type) where
  RunFieldsType m '[API.Field ksI t] = API.Field ksI t
  RunFieldsType m '[a :> b] = a :> b
  RunFieldsType m ((API.Field ksJ t) ': rest) = API.Field ksJ t :<> RunFieldsType m rest
  RunFieldsType m ((a :> b) ': rest) = (a :> b) :<> RunFieldsType m rest
  RunFieldsType m a = TypeError (
    'Text "All field entries in an Object must be Field or Argument :> Field. Got: " ':<>: 'ShowType a)

-- Match the three possible cases for Fields (see also RunFieldsType)
type family RunFieldsHandler (m :: Type -> Type) (a :: Type) = (r :: Type) where
  RunFieldsHandler m (f :<> fs) = FieldHandler m (FieldResolverDispatchType f) :<> RunFieldsHandler m fs
  RunFieldsHandler m (API.Field ksL t) = FieldHandler m (FieldResolverDispatchType (API.Field ksL t))
  RunFieldsHandler m (a :> b) = FieldHandler m (FieldResolverDispatchType (a :> b))
  RunFieldsHandler m a = TypeError (
    'Text "Unexpected RunFieldsHandler types: " ':<>: 'ShowType a)


class RunFields m a where
  -- | Run a single 'Selection' over all possible fields (as specified by the
  -- type @a@), returning exactly one 'GValue.ObjectField' when a field
  -- matches, or an error otherwise.
  --
  -- Individual implementations are responsible for calling 'runFields' if
  -- they haven't matched the field and there are still candidate fields
  -- within the handler.
  runFields :: RunFieldsHandler m a -> Field Value -> m ResolveFieldResult

instance forall f fs m dispatchType.
         ( BuildFieldResolver m dispatchType
         , dispatchType ~ FieldResolverDispatchType f
         , RunFields m fs
         , KnownSymbol (FieldName dispatchType)
         , Monad m
         ) => RunFields m (f :<> fs) where
  runFields (handler :<> nextHandlers) field =
    resolveField @dispatchType @m handler nextHandler field
    where
      nextHandler = runFields @m @fs nextHandlers field

instance forall ksM t m dispatchType.
         ( BuildFieldResolver m dispatchType
         , KnownSymbol ksM
         , dispatchType ~ FieldResolverDispatchType (API.Field ksM t)
         , Monad m
         ) => RunFields m (API.Field ksM t) where
  runFields handler field =
    resolveField @dispatchType @m handler nextHandler field
    where
      nextHandler = pure (Result [FieldNotFoundError (getName field)] Nothing)

instance forall m a b dispatchType.
         ( BuildFieldResolver m dispatchType
         , dispatchType ~ FieldResolverDispatchType (a :> b)
         , KnownSymbol (FieldName dispatchType)
         , Monad m
         ) => RunFields m (a :> b) where
  runFields handler field =
    resolveField @dispatchType @m handler nextHandler field
    where
      nextHandler = pure (Result [FieldNotFoundError (getName field)] Nothing)

instance forall typeName interfaces fields m.
         ( RunFields m (RunFieldsType m fields)
         , API.HasObjectDefinition (API.Object typeName interfaces fields)
         , Monad m
         ) => HasResolver m (API.Object typeName interfaces fields) where
  type Handler m (API.Object typeName interfaces fields) = m (RunFieldsHandler m (RunFieldsType m fields))

  resolve _ Nothing = throwE MissingSelectionSet
  resolve mHandler (Just selectionSet) =
    case getSelectionSet of
      Left err -> throwE err
      Right ss -> do
        -- Run the handler so the field resolvers have access to the object.
        -- This (and other places, including field resolvers) is where user
        -- code can do things like look up something in a database.
        handler <- mHandler
        r <- traverse (runFields @m @(RunFieldsType m fields) handler) ss
        let (Result errs obj)  = GValue.objectFromOrderedMap . OrderedMap.catMaybes <$> sequenceA r
        pure (Result errs (GValue.ValueObject obj))

    where
      getSelectionSet = do
        defn <- first SchemaError $ API.getDefinition @(API.Object typeName interfaces fields)
        -- Fields of a selection set may be behind "type conditions", due to
        -- inline fragments or the use of fragment spreads. These type
        -- conditions are represented in the schema by the name of a type
        -- (e.g. "Dog"). To determine which type conditions (and thus which
        -- fields) are relevant for this 1selection set, we need to look up the
        -- actual types they refer to, as interfaces (say) match objects
        -- differently than unions.
        --
        -- See <https://facebook.github.io/graphql/#sec-Field-Collection> for
        -- more details.
        (SelectionSet ss') <- first ValidationError $ getSelectionSetForType defn selectionSet
        pure ss'

-- TODO(tom): we're getting to a point where it might make sense to
-- split resolver into submodules (GraphQL.Resolver.Union  etc.)


-- | For unions we need a way to have type-safe, open sum types based
-- on the possible 'API.Object's of a union. The following closed type
-- family selects one Object from the union and returns the matching
-- 'HasResolver' 'Handler' type. If the object @o@ is not a member of
-- 'API.Union' then the user code won't compile.
--
-- This type family is an implementation detail but its TypeError
-- messages are visible at compile time.
type family TypeIndex (m :: Type -> Type) (object :: Type) (union :: Type) = (result :: Type) where
  TypeIndex m (API.Object name interfaces fields) (API.Union uName (API.Object name interfaces fields:_)) =
    Handler m (API.Object name interfaces fields)
  TypeIndex m (API.Object name interfaces fields) (API.Union uName (API.Object name' i' f':objects)) =
    TypeIndex m (API.Object name interfaces fields) (API.Union uName objects)
  -- Slightly nicer type errors:
  TypeIndex _ (API.Object name interfaces fields) (API.Union uName '[]) =
    TypeError ('Text "Type not found in union definition: " ':<>: 'ShowType (API.Object name interfaces fields))
  TypeIndex _ (API.Object name interfaces fields) x =
    TypeError ('Text "3rd type must be a union but it is: " ':<>: 'ShowType x)
  TypeIndex _ o _ =
    TypeError ('Text "Invalid TypeIndex. Must be Object but got: " ':<>: 'ShowType o)


-- | The 'Handler' type of a 'API.Union' must be the same for all
-- possible Objects, but each Object has a different type. We
-- unsafeCoerce the return type into an Any, tagging it with the union
-- and the underlying monad for type safety, but we elide the Object
-- type itself. This way we can represent all 'Handler' types of the
-- Union with a single type and still stay type-safe.
type role DynamicUnionValue representational representational
data DynamicUnionValue (union :: Type) (m :: Type -> Type) = DynamicUnionValue { _label :: Text, _value :: GHC.Exts.Any }

class RunUnion m union objects where
  runUnion :: DynamicUnionValue union m -> SelectionSetByType Value -> m (Result Value)

instance forall m union objects name interfaces fields.
  ( Monad m
  , KnownSymbol name
  , TypeIndex m (API.Object name interfaces fields) union ~ Handler m (API.Object name interfaces fields)
  , RunFields m (RunFieldsType m fields)
  , API.HasObjectDefinition (API.Object name interfaces fields)
  , RunUnion m union objects
  ) => RunUnion m union (API.Object name interfaces fields:objects) where
  runUnion duv selectionSet =
    case extractUnionValue @(API.Object name interfaces fields) @union @m duv of
      Just handler -> resolve @m @(API.Object name interfaces fields) handler (Just selectionSet)
      Nothing -> runUnion @m @union @objects duv selectionSet

-- AFAICT it should not be possible to ever hit the empty case because
-- the compiler doesn't allow constructing a unionValue that's not in
-- the Union. If the following code ever gets executed it's almost
-- certainly a bug in the union code.
--
-- We still need to implement this instance for the compiler because
-- it exhaustively checks all cases when deconstructs the Union.
instance forall m union. RunUnion m union '[] where
  runUnion (DynamicUnionValue label _) selection =
    panic ("Unexpected branch in runUnion, got " <> show selection <> " for label " <> label <> ". Please file a bug.")

instance forall m unionName objects.
  ( Monad m
  , KnownSymbol unionName
  , RunUnion m (API.Union unionName objects) objects
  ) => HasResolver m (API.Union unionName objects) where
  type Handler m (API.Union unionName objects) = m (DynamicUnionValue (API.Union unionName objects) m)
  resolve _ Nothing = throwE MissingSelectionSet
  resolve mHandler (Just selectionSet) = do
    duv <- mHandler
    runUnion @m @(API.Union unionName objects) @objects duv selectionSet

symbolText :: forall ks. KnownSymbol ks => Text
symbolText = toS (symbolVal @ks Proxy)

-- | Translate a 'Handler' into a DynamicUnionValue type required by
-- 'Union' handlers. This is dynamic, but nevertheless type-safe
-- because we can only tag with types that are part of the union.
--
-- Use e.g. like "unionValue @Cat" if you have an object like this:
--
-- >>> type Cat = API.Object "Cat" '[] '[API.Field "name" Text]
--
-- and then use `unionValue @Cat (pure (pure "Felix"))`. See
-- `examples/UnionExample.hs` for more code.
unionValue ::
  forall (object :: Type) (union :: Type) m (name :: Symbol) interfaces fields.
  (Monad m, API.Object name interfaces fields ~ object, KnownSymbol name)
  => TypeIndex m object union -> m (DynamicUnionValue union m)
unionValue x =
  -- TODO(tom) - we might want to move to Typeable `cast` for uValue
  -- instead of doing our own unsafeCoerce because it comes with
  -- additional safety guarantees: Typerep is unforgeable, while we
  -- can still into a bad place by matching on name only. We can't
  -- actually segfault this because right now we walk the list of
  -- objects in a union left-to-right so in case of duplicate names we
  -- only every see one type. That doesn't seen like a great thing to
  -- rely on though!

  -- Note that unsafeCoerce is safe because we index the type from the
  -- union with an 'API.Object' whose name we're storing in label. On
  -- the way out we check that the name is the same, and we know the
  -- type universe is the same because we annotated DynamicUnionValue
  -- with the type universe.
  pure (DynamicUnionValue (symbolText @name) (unsafeCoerce x))

extractUnionValue ::
  forall (object :: Type) (union :: Type) m (name :: Symbol) interfaces fields.
  (API.Object name interfaces fields ~ object, KnownSymbol name)
  => DynamicUnionValue union m -> Maybe (TypeIndex m object union)
extractUnionValue (DynamicUnionValue uName uValue) =
  if uName == symbolText @name
  then Just (unsafeCoerce uValue)
  else Nothing
