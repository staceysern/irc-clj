IRC Server
==========

An IRC server written in Clojure.

This IRC server supports a subset of the RFC 2812 client protocol and has
been designed to mimic the behavior of freenode servers.

Invocation
----------

lein launch [-p port (default=6667)]

Unit Tests
----------

Midje is used for unit testing.  To run all the unit tests:

lein midje
