# graphql-api

[![CircleCI](https://circleci.com/gh/jml/graphql-api.svg?style=shield)](https://circleci.com/gh/jml/graphql-api)
[![Documentation Status](https://readthedocs.org/projects/haskell-graphql-api/badge/?version=latest)](http://haskell-graphql-api.readthedocs.io/en/latest/?badge=latest)

`graphql-api` helps you implement a robust [GraphQL](http://graphql.org/) API in Haskell. By the time a query makes it to your handler you are dealing with strong, static types that make sense for your problem domain. All your handlers are normal Haskell functions because we derive their type signature from the schema. If you have used [servant](http://haskell-servant.readthedocs.io/en/stable/), this will sound familiar.

The library provides type combinators to create a GraphQL schema, and functions to parse and evaluate queries against the schema.

You can find the latest release on [hackage](https://hackage.haskell.org/package/graphql-api).

We implement the [GraphQL specification](https://facebook.github.io/graphql/) as best as we can in Haskell. We figure they know what they're doing. Even if an alternative API or behaviour looks nicer, we will defer to the spec.

## Example

Say we have a simple GraphQL schema like:

```graphql
type Hello {
  greeting(who: String!): String!
}
```

which defines a single top-level type `Hello` which contains a single field, `greeting`, that takes a single, required argument `who`.

We can define this schema in Haskell and implement a simple handler like so:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text (Text)
import Data.Monoid ((<>))

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler)

type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text ]

hello :: Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

run :: Text -> IO Response
run = interpretAnonymousQuery @Hello hello
```

We require GHC 8.0.2 or later for features like the `@Hello` type application, and for certain bug fixes.

With the code above we can now run a query:

```haskell
run "{ greeting(who: \"mort\") }"
```

Which will produce the following GraphQL response:

```json
{
  "data": {
    "greeting": "Hello mort"
  }
}
```

## Status

Our current goal is to gather feedback. We have learned a lot about GraphQL in the course of making this library, but we don't know what a good GraphQL library looks like in Haskell. Please [let us know](https://github.com/jml/graphql-api/issues/new) what you think. We won't mind if you file a bug telling us how good the library is.

Because we're still learning, we make **no** guarantees about API stability, or anything at all really.

We are tracking open problems, missing features & wishlist items in [GitHub's issue tracker](https://github.com/jml/graphql-api/issues).

## Roadmap

* Near future:
  - Better error messages (this is really important to us)
  - Full support for recursive data types
  - Close off loose ends in current implementation & gather feedback
* Medium future:
  - Full schema validation
  - Schema introspection
  - Stabilize public API
* Long term:
  - Derive client implementations from types
  - Allow users to implement their own type combinators

## References

* [GraphQL Specification](http://facebook.github.io/graphql/) ([source](https://github.com/facebook/graphql))
* [GraphQL tutorial](http://graphql.org/learn/)
* [GraphQL AST in Haskell](http://hackage.haskell.org/package/graphql-0.3/docs/Data-GraphQL-AST.html)

## Copyright

All files Copyright (c) 2016-2017 Thomas E. Hunger & Jonathan M. Lange, except:

* src/GraphQL/Internal/Syntax/AST.hs
* src/GraphQL/Internal/Syntax/Encoder.hs
* src/GraphQL/Internal/Syntax/Parser.hs

for which see LICENSE.BSD3 in this repository.
