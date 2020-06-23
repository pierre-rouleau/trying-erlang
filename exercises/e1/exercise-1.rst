=======================================
Exercise 1 - Palindrome Checking Server
=======================================


This contains work and notes related to the `first exercise`_.

I have written the following 2 files.

Simple Basic Single-Client Server
=================================

- File: palind.erl_

This one is a basic, single client server that can
only reply to the instantiating process.  Used with the shell it forces
manipulations to extract the replies.  It exposes only one function: the
server itself.

Here's a log of the compilation, spawning, using and stopping the server::

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palind}
    2> ShellPid = self().
    ShellPid = self().
    <0.79.0>
    3> Server = spawn(palind, server, [ShellPid]).
    Server = spawn(palind, server, [ShellPid]).
    <0.87.0>
    4> Server ! {check, "Madam I\'m Adam"}.
    Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    5> Server ! {check, "abc"}.
    Server ! {check, "abc"}.
    {check,"abc"}
    6> flush().
    flush().
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"abc\" is not a palindrome."}
    ok
    7> Server ! stop.
    Server ! stop.
    Server stopped.
    stop
    8> flush().
    flush().
    ok
    9>


Flexible and Hidden Server
==========================

- File: epalind.erl

This  does a little bit more by encapsulating the protocol between a
client and a server.  This implementation hides the server and exposes a
single function that hides the client/server implementation: the function
takes a string and returns a tuple of {status, string} that describes a
success or failure.

I have not yet written that file.  I'll do that soon.


.. _first exercise: https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334
.. _palind.erl:     palind.erl


..
   -----------------------------------------------------------------------------
