# Defining GraphQL type APIs

First some imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Introduction where

import Protolude

import GraphQL.TypedSchema (Object, Field, Argument, (:>), (:<>)(..))
import GraphQL.TypedApi (HandlerType)
```

The core idea for this library is that we define a composite type that
specifies the whole API, and then implement a matching handler.

The main GraphQL entities we care about are Objects and Fields. Each
Field can have arguments.

``` haskell
type HelloWorld = Object "HelloWorld" '[] '[Argument "greeting" Text :> Field "me" Text]
```

The example above is equivalent to the following GraphQL type:

```
type HelloWorld {
  me(greeting: String!): String!
}
```

And if we imagine implementation (more later) we could query it like this:

```
{ me(greeting: "hello") }
```

## The handler

We defined a corresponding handler via the `HandlerType m a` which takes
the monad to run in (`IO` in this case) and the actual API definition
(`HelloWorld`).

```haskell
handler :: HandlerType IO HelloWorld
handler = pure $ (\greeting -> pure ("Hello " <> greeting)) :<> ()
```

The implementation looks slightly weird, but it's weird for good
reasons. In order:

* The first `pure` allows us to run actions in the base monad (`IO`
here) before returning anything. This is useful to allocate a resource like a database connection.
* The `pure` in the function call allows us to **avoid running
actions** when the field hasn't been requested. We only run the action
if the query specifies the field. Note that lazy evaluation would not
work because of the monadic nature of handlers.
* Finally, we have to terminate each handler with `:<> ()`. This is an
implementation artifact which we'd prefer to avoid but can not at the
moment.
