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


Here's a log of the compilation, spawning, using and stopping the server:

.. code:: erlang

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

And some more after placing loop recursion where it belongs: at the end of
the function, not inside the case statement.

.. code:: erlang

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palind}
    2> Self = self().
    Self = self().
    <0.79.0>
    3> Server = spawn(palind, server, [Self]).
    Server = spawn(palind, server, [Self]).
    <0.87.0>
    4> Server ! {check, "abc"}.
    Server ! {check, "abc"}.
    {check,"abc"}
    5> Server ! {check, "def"}.
    Server ! {check, "def"}.
    {check,"def"}
    6> Server ! {check, "Madam I\'m Adam"}.
    Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    7> Server ! {check, "Madam I\'m not Adam"}.
    Server ! {check, "Madam I\'m not Adam"}.
    {check,"Madam I'm not Adam"}
    8> flush().
    flush().
    Shell got {result,"\"abc\" is not a palindrome."}
    Shell got {result,"\"def\" is not a palindrome."}
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"Madam I'm not Adam\" is not a palindrome."}
    ok
    9> Server ! {check, "oh! ho!"}.
    Server ! {check, "oh! ho!"}.
    {check,"oh! ho!"}
    10> Server ! {check, "oh! no!"}.
    Server ! {check, "oh! no!"}.
    {check,"oh! no!"}
    11> flush().
    flush().
    Shell got {result,"\"oh! ho!\" is not a palindrome."}
    Shell got {result,"\"oh! no!\" is not a palindrome."}
    ok
    12> Server ! {check, "Never odd or even"}.
    Server ! {check, "Never odd or even"}.
    {check,"Never odd or even"}
    13> flush().
    flush().
    Shell got {result,"\"Never odd or even\" is a palindrome"}
    ok
    14>

.. _first exercise: https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334
.. _palind.erl:     palind.erl


Well, the first version worked better since it stopped the server properly.
But It let invalid messages accumulate in the server's mailbox.
So I updated it again, and made sure to discard invalid messages and to stop
the server when asked.

The session with the new instance is shown here:

.. code:: erlang


    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palind}
    2> Server = spawn(palind, server, [self()]).
    Server = spawn(palind, server, [self()]).
    <0.86.0>
    3> Server ! "invalidly formatted message".
    Server ! "invalidly formatted message".
    "invalidly formatted message"
    4> Server ! {check, "ahha"}.
    Server ! {check, "ahha"}.
    {check,"ahha"}
    5> Server ! {check, "never odd or even"}.
    Server ! {check, "never odd or even"}.
    {check,"never odd or even"}
    6> Server ! {check, "Madam I\'m Adam"}.
    Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    7> Server ! {check, "abc"}.
    Server ! {check, "abc"}.
    {check,"abc"}
    8> flush().
    flush().
    Shell got {result,"\"ahha\" is a palindrome"}
    Shell got {result,"\"never odd or even\" is a palindrome"}
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"abc\" is not a palindrome."}
    ok
    9> Server ! stop.
    Server ! stop.
    Server stopped.
    stop
    10> Server ! {check, "abc"}.
    Server ! {check, "abc"}.
    {check,"abc"}
    11> flush().
    flush().
    ok
    12>


Looking Back
------------

One aspect of Erlang I find difficult is to remember how to separate and
terminate statements.  It's a problem that never occurs in Algol-derived
or so call curly-brace programming languages.  Lisp-like programming languages
also don't suffer from this: use a Lisp aware editor and you can depend on it
to properly highlight the S-expressions and balance the parentheses.

At first I thought my first version of the code had a bug in it because the
loop call was inside the case statement.  I also thought that int the second
version.  But instead I prevented the server to stop.
The version 3 of the file handles invalid messages, removing them from the
server mailbox, and then loops when valid and invalid messages are received,
it does not loop when a stop requested is received.




Flexible and Hidden Server
==========================

The second implementation uses 2 files:

- File 1: palindc.erl_  : the client code
- File 2: palinds.erl_   : the server code

This  does a little bit more by encapsulating the protocol between a
client and a server.  This implementation hides the server and exposes a
single function that hides the client/server implementation: the function
takes a string and returns a tuple of {status, string} that describes a
success or failure.



.. _palindc.erl: palindc.erl
.. _palinds.erl: palinds.erl


..
   -----------------------------------------------------------------------------
