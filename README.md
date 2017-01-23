# graphql-api

[![CircleCI](https://circleci.com/gh/jml/graphql-api.svg?style=shield)](https://circleci.com/gh/jml/graphql-api)
[![Documentation Status](https://readthedocs.org/projects/haskell-graphql-api/badge/?version=latest)](http://haskell-graphql-api.readthedocs.io/en/latest/?badge=latest)

This library provides type combinators to create a GraphQL schema, and functions to parse and evaluate queries against the schema. It is inspired by [servant](http://haskell-servant.readthedocs.io/en/stable/) but the two projects don't share any code.

More specifically, `graphql-api` helps with implementing a robust GraphQL API in Haskell. By the time a query makes it to your handler you are dealing with strong, static types. All handlers are normal Haskell functions because we derive their type signature from the schema.

You can find the latest release on [hackage](https://hackage.haskell.org/package/graphql-api).

Note that we're trying to implement the GraphQL standard as best as we can in Haskell. I.e. even if an alternative API or behaviour looks nicer we will defer to the standard.

## Hello world example

Below we define a `Hello` Schema that contains a single field, `greeting`, that takes a single, required argument `who`:

```haskell
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

Note that we require GHC 8.0.2 or later for features like the `@Hello` type application, and for certain bug fixes.

With the code above we can now run a query:

```haskell
run "{ greeting(who: \"mort\") }"
```

## Current status

This first release's goal is to gather feedback. We make **no** guarantees about API stability, or anything at all really.

We are tracking open problem, missing features & wishlist-items in [GitHub's issue tracker](https://github.com/jml/graphql-api/issues).

## Roadmap

* Near future:
  - Complete lose ends in current implementation & gather feedback.
* Medium future:
  - Implement introspection
* Long term:
  - Derive client implementations from types
  - Allow users to implement their own type combinators

## References

* [GraphQL Specification](http://facebook.github.io/graphql/) ([source](https://github.com/facebook/graphql))
* [GraphQL tutorial](http://graphql.org/learn/)
* [GraphQL AST in Haskell](http://hackage.haskell.org/package/graphql-0.3/docs/Data-GraphQL-AST.html)

## Copyright

All files Copyright (c) 2016 Thomas E. Hunger & Jonathan M. Lange, except:

* src/GraphQL/Internal/AST.hs
* src/GraphQL/Internal/Encoder.hs
* src/GraphQL/Internal/Parser.hs

for which see LICENSE.BSD3 in this repository.
