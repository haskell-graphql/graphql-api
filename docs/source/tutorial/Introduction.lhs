# Defining GraphQL type APIs

First some imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Introduction where

import Protolude

import System.Random

import GraphQL
import GraphQL.API (Object, Field, Argument, (:>), Union)
import GraphQL.Resolver (Handler, (:<>)(..), unionValue)
```

## A simple GraphQL service

A [GraphQL](http://graphql.org/) service is made up of two things:

 1. A schema that defines the service
 2. Some code that implements the service's behavior

We're going to build a very simple service that says hello to
people. Our GraphQL schema for this looks like:

```graphql
type Hello {
  greeting(who: String!): String!
}
```

Which means we have base type, an _object_ called `Hello`, which has a single
_field_ `greeting`, which takes a non-nullable `String` called `who` and
returns a `String`.

Note that all the types here are GraphQL types, not Haskell types. `String`
here is a GraphQL `String`, not a Haskell one.

And we want to be able to send queries that look like:

```graphql
{
  greeting(who: "world")
}
```

And get responses like:

```json
{
  data: {
    greeting: "Hello world!"
  }
}
```

### Defining the schema

Here's how we would define the schema in Haskell:

```haskell
type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text
   ]
```

Breaking this down, we define a new Haskell type `Hello`, which is a GraphQL
object (also named `"Hello"`) that implements no interfaces (hence `'[]`). It
has one field, called `"greeting"` which returns some `Text` and takes a
single named argument `"who"`, which is also `Text`.

Note that the GraphQL `String` from above got translated into a Haskell
`Text`.

There are some noteworthy differences between this schema and the GraphQL
schema:

* The GraphQL schema requires a special annotation to say that a value cannot
  be null, `!`. In Haskell, we instead assume that nothing can be null.
* In the GraphQL schema, the argument appears *after* the field name. In
  Haskell, it appears *before*.
* In Haskell, we name the top-level type twice, once on left hand side of the
  type definition and once on the right.

### Implementing the handlers

Once we have the schema, we need to define the corresponding handlers, which
are `Handler` values.

Here's a `Handler` for `Hello`:

```haskell
hello :: Handler IO Hello
hello = pure greeting
  where
    greeting who = pure ("Hello " <> who <> "!")
```

The type signature, `Handler IO Hello` shows that it's a `Handler` for
`Hello`, and that it runs in the `IO` monad. (Note: nothing about this example
code requires the `IO` monad, it's just a monad that lots of people has heard
of.)

The implementation looks slightly weird, but it's weird for good reasons.

The first layer of the handler, `pure greeting`, produces the `Hello` object.
The `pure` might seem redundant here, but making this step monadic allows us
to run actions in the base monad.

The second layer of the handler, the implementation of `greeting`, produces
the value of the `greeting` field. It is monadic so that it will only be
executed when the field was requested.

Each field handler is a separate monadic action so we only perform the side
effects for fields present in the query.

This handler is in `Identity` because it doesn't do anything particularly
monadic. It could be in `IO` or `STM` or `ExceptT Text IO` or whatever you
would like.

### Running queries

Defining a service isn't much point unless you can query. Here's how:

```haskell
queryHello :: IO Response
queryHello = interpretAnonymousQuery @Hello hello "{ greeting(who: \"mort\") }"
```

The actual `Response` type is fairly verbose, so we're most likely to turn it
into JSON:

```
λ Aeson.encode <$> queryHello
"{\"greeting\":\"Hello mort!\"}"
```

## Combining field handlers with :<>

How do we define an object with more than one field?

Let's implement a simple calculator that can add and subtract integers. First,
the schema:

```graphql
type Calculator {
  add(a: Int!, b: Int!): Int!,
  sub(a: Int!, b: Int!): Int!,
}
```

Here, `Calculator` is an object with two fields: `add` and `sub`.

And now the Haskell version:

``` haskell
type Calculator = Object "Calculator" '[]
  '[ Argument "a" Int32 :> Argument "b" Int32 :> Field "add" Int32
   , Argument "a" Int32 :> Argument "b" Int32 :> Field "subtract" Int32
   ]
```

So far, this is the same as our `Hello` example.

And its handler:

```haskell
calculator :: Handler IO Calculator
calculator = pure (add :<> subtract')
  where
    add a b = pure (a + b)
    subtract' a b = pure (a - b)
```

This handler introduces a new operator, `:<>` (pronounced "birdface"), which
is used to compose two existing handlers into a new handler. It's inspired by
the operator for monoids, `<>`.

Note that we still need `pure` for each individual handler.

## Nesting Objects

How do we define objects made up other objects?

One of the great things in GraphQL is that objects can be used as types for
fields. Take this classic GraphQL schema as an example:

```graphql
type Query {
  me: User!
}

type User {
  name: Text!
}
```

We would query this schema with something like:

```graphql
{
  me {
    name
  }
}
```

Which would produce output like:

```json
{
  data: {
    me: {
      name: "Mort"
    }
  }
}
```

The Haskell type for this schema looks like:

```haskell
type User = Object "User" '[] '[Field "name" Text]
type Query = Object "Query" '[] '[Field "me" User]
```

Note that `Query` refers to the type `User` when it defines the field `me`.

We write nested handlers the same way we write the top-level handler:

```haskell
user :: Handler IO User
user = pure name
  where
    name = pure "Mort"

query :: Handler IO Query
query = pure user
```

And that's it.

## Unions

GraphQL has [support for union
types](http://graphql.org/learn/schema/#union-types). These require special
treatment in Haskell.

Let's define a union, first in GraphQL:

```graphql
union UserOrCalculator = User | Calculator
```

And now in Haskell:

```haskell
type UserOrCalculator = Union "UserOrCalculator" '[User, Calculator]
```

And let's define a very simple top-level object that uses `UserOrCalculator`:

```haskell
type UnionQuery = Object "UnionQuery" '[] '[Field "union" UserOrCalculator]
```

and a handler that randomly returns either a user or a calculator:

```haskell
unionQuery :: Handler IO UnionQuery
unionQuery = do
  returnUser <- randomIO
  if returnUser
  then pure (unionValue @User user)
  else pure (unionValue @Calculator calculator)
```

The important thing here is that we have to wrap the actual objects we return
using `unionValue`.

Note that while `unionValue` looks a bit like `unsafeCoerce` by forcing one
type to become another type, it's actually type-safe because we use a
*type-index* to pick the correct type from the union. Using e.g. `unionValue
@HelloWorld handler` will not compile because `HelloWorld` is not in the
union.

## Where next?

We have an
[examples](https://github.com/jml/graphql-api/tree/master/tests/Examples)
directory showing full code examples.

We also have a fair number of [end-to-end
tests](https://github.com/jml/graphql-api/tree/master/tests/EndToEndTests.hs)
based on an [example
schema](https://github.com/jml/graphql-api/tree/master/tests/ExampleSchema.hs)
that you might find interesting.

If you want to try the examples in this tutorial you can run:

```bash
stack repl tutorial
```
