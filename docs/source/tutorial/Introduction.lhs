# Defining GraphQL type APIs

First some imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Introduction where

import Protolude

import GraphQL.TypedSchema
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
