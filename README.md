ESDL2
=====

SDL2 Erlang NIF.

Status
------

Week-end project. Work in progress.

The following limitations apply:

 *  Erlang 17.0+ is required
 *  SDL 2.0.3+ is required
 *  No support for UTF-8 strings, only Latin-1

The following ideas need to be investigated:

 *  We may benefit from the reference receive optimization when doing calls
 *  We may want a way to pipeline draw operations
