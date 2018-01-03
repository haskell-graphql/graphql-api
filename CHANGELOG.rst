=====================
graphql-api changelog
=====================

$next (yyyy-mm-dd)
==================

Breaking changes
----------------

* ``Enum`` handlers are now monadic (see `#118`_)

.. _`#118`: https://github.com/jml/graphql-api/issues/118


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
