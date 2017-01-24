# Defining GraphQL type APIs

First some imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Introduction where

import Protolude

import GraphQL
import GraphQL.API (Object, Field, Argument, (:>))
import GraphQL.Resolver (Handler, (:<>)(..))
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

Let's implement a simple calculator that cann add and subtract integers:

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
calculator = pure (add :<> subtract)
  where
  add a b = pure (a + b)
  subtract a b = pure (a - b)
```

Note that we still need `pure` for each individual handler.


##
