# Defining GraphQL type APIs

First some imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Introduction where

import Protolude

import GraphQL
import GraphQL.API (Object, Field, Argument, (:>), Union)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)
```

The core idea for this library is that we define a composite type that
specifies the whole API, and then implement a matching handler.

The main GraphQL entities we care about are Objects and Fields. Each
Field can have arguments.

``` haskell
type HelloWorld = Object "HelloWorld" '[]
  '[ Argument "greeting" Text :> Field "me" Text
   ]
```

The example above is equivalent to the following GraphQL type:

```
type HelloWorld {
  me(greeting: String!): String!
}
```

And if we had a code to handle that type (more later) we could query it like this:

```
{ me(greeting: "hello") }
```

## Implementing a handler

We define a corresponding handler via the `Handler m a` which takes
the monad to run in (`IO` in this case) and the actual API definition
(`HelloWorld`):

```haskell
handler :: Handler IO HelloWorld
handler = pure (\greeting -> pure (greeting <> " to me"))
```

The implementation looks slightly weird, but it's weird for good
reasons. In order:

* The first `pure` allows us to run actions in the base monad (here `IO`
 before returning anything. This is useful to allocate a resource
like a database connection.
* The `pure` in the function call allows us to **avoid running
actions** when the field hasn't been requested: Each handler is a
separate monadic action so we only perform the side effects for fields
present in the query.


## Combining field handlers with :<>

Let's implement a simple calculator that can add and subtract integers:

``` haskell
type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int32 :> Argument "b" Int32 :> Field "add" Int32
   , Argument "a" Int32 :> Argument "b" Int32 :> Field "subtract" Int32
   ]
```

Every element in a list in Haskell has the same type, so we can't
really return a list of different handlers. Instead we compose the
different handlers with a new operator, `:<>`. This operator, commonly
called birdface, is based on the operator for monoids, `<>`.

``` haskell
calculator :: Handler IO Calculator
calculator = pure (add :<> subtract')
  where
  add a b = pure (a + b)
  subtract' a b = pure (a - b)
```

Note that we still need `pure` for each individual handler.


## Nesting Objects

Objects can be used as a type in fields. This allows us to implement a
server for the classic GraphQL example query:


```
{
  me { name }
}
```

The Haskell schema for that looks like this:

``` haskell
type User = Object "User" '[] '[Field "name" Text]
type Query = Object "Query" '[] '[Field "me" User]
```

Note the type `User` for `me`.


We write nested handlers the same way we write the top-level handler:

``` haskell
user :: Handler IO User
user = pure (pure "mort")

query :: Handler IO Query
query = pure user
```

## Unions

Union handlers require special treatment in Haskell because we need to
return the same type for each possible, different type in the union.

Let's define a union:

``` haskell
type UserOrCalcualtor = Union "UserOrCalcualtor" '[User, Calculator]
type UnionQuery = Object "UnionQuery" '[] '[Field "union" UserOrCalcualtor]
```

and a handler that returns a user:

``` haskell
unionQuery :: Handler IO UnionQuery
unionQuery = pure (unionValue @User user)
```

Note that, while `unionValue` looks a bit like `unsafeCoerce` by
forcing one type to become another type, it's actually type-safe
because we use a *type-index* to pick the correct type from the
union. Using e.g. `unionValue @HelloWorld handler` will not compile
because `HelloWorld` is not in the union.

## Running a query

```haskell
hello :: IO Response
hello = interpretAnonymousQuery @HelloWorld handler "{ me(greeting: \"hey ho\") }"
```

But our output is pretty long:

```
λ hello
Success (Object' (OrderedMap {keys = [Name {unName = "me"}], toMap = fromList [(Name {unName = "me"},ValueScalar' (ConstString (String "hey ho  to me")))]}))
```

The output object `Object'` has a `ToJSON` instance:

```
λ map (\(Success o) -> Aeson.encode o) hello
"{\"me\":\"hey ho to me\"}"
```


## Where next?

We have an
[examples](https://github.com/jml/graphql-api/tree/master/tests/Examples)
directory showing full code examples.

If you want to try the examples in this tutorial you can run:

```bash
stack repl tutorial
```
