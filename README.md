IRC Server
==========

An IRC server written in Clojure.  

This project is under construction.  The core server functionality is in 
place.  Each user is represented by a pair of core.async channels through
which IRC messages pass.

The code to manage the TCP connections and pass messages to and from the
core.async channels has yet to be written.

Unit Tests
----------
 
Midje is used for unit testing.  To run all the unit tests:
 
lein midje






