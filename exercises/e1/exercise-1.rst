=======================================
Exercise 1 - Palindrome Checking Server
=======================================


This contains work and notes related to the `first exercise`_.

I am writing 2 files:

#. palind.erl_ This one is a basic, single client server that can
only reply to the instantiating process.  Used with the shell it forces
manipulations to extract the replies.  It exposes only one funciton: the
server itself.

#. epalind.erl does a little bit more by encapsulating the protocol between a
   client and a server.  This implementation hides the server and exposes a
   single function that hides the client/server implementation: the function
   takes a string and returns a tuple of {status, string} that describes a
   success or failure.

   I have not yet written that file.  I'll do that soon.


.. _first exercise: https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334
.. _palind.erl:     palind.erl


..
   -----------------------------------------------------------------------------
