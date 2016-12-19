# graphql-api

[![CircleCI](https://circleci.com/gh/jml/graphql-api.svg?style=shield)](https://circleci.com/gh/jml/graphql-api)

Sketch of GraphQL stuff

## What it is

Aim is to be [servant](http://haskell-servant.readthedocs.io/) for GraphQL.

To do this, we're going to need:

* Type-level definition of queries and schemas
* Evaluation of GraphQL queries (and mutations) according to those types.

We can build off the
existing [graphql](http://hackage.haskell.org/package/graphql) library for
parsing & representing queries.

## Why you might want it

Right now, you don't. We're working on it. Please feel free to contribute by
filing issues & submitting PRs.

## References

* [GraphQL Specification](http://facebook.github.io/graphql/) ([source](https://github.com/facebook/graphql))
* [GraphQL tutorial](http://graphql.org/learn/)
* [GraphQL AST in Haskell](http://hackage.haskell.org/package/graphql-0.3/docs/Data-GraphQL-AST.html)
