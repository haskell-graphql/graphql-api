=====================
graphql-api changelog
=====================

0.4.0 (YYYY-MM-DD)
==================

* Schemas that have empty field lists or empty unions will fail much earlier

0.3.0 (2018-02-08)
==================

Breaking changes
----------------

* ``Enum`` handlers are now monadic (see `#118`_)
* You must use protolude 0.2.1 or later
* ``Defaultable`` must now be imported from ``GraphQL.API``, rather than ``GraphQL.Resolver``,
  this moves ``GraphQL.API`` closer to being sufficient for API definition. (see `#149`_)
* ``GraphQL.Value.ToValue`` and ``GraphQL.Value.FromValue`` modules have been removed.
  Import ``ToValue(..)`` and ``FromValue(..)`` from ``GraphQL.Value`` directly.

Improvements
------------

* Now support GHC 8.2 as well as 8.0.2 and later
* Added support for anonymous queries (thanks `@sunwukonga`_)

.. _`#118`: https://github.com/jml/graphql-api/issues/118
.. _`#149`: https://github.com/haskell-graphql/graphql-api/issues/149
.. _`@sunwukonga`: https://github.com/sunwukonga

v0.2.0 (2017-10-12)
===================

* Make ``Name`` an overloaded string that panics if an invalid name is
  provided.
* Correctly descend into the type parameter of a ``Maybe``. See https://github.com/jml/graphql-api/issues/119.
  This is a backwards-incompatible change.

  A common update would be having to ``fmap pure callback`` instead of just ``callback``
  for ``Maybe`` handlers.


v0.1.0 (2017-01-30)
===================

No code changes.

* Remove ``-Werror`` in order to upload to hackage


v0.1.0 (2017-01-29)
===================

Initial release, support basic handling of GraphQL queries.
