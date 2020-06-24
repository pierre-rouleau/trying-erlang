=======================================
Exercise 1 - Palindrome Checking Server
=======================================


This contains work and notes related to the `first exercise`_.

I have written the following 2 sets of files, explained in the first and
second section.

.. contents::  **Table Of Contents**
.. sectnum::



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
    {ok,palind}
    2> ShellPid = self().
    <0.79.0>
    3> Server = spawn(palind, server, [ShellPid]).
    <0.87.0>
    4> Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    5> Server ! {check, "abc"}.
    {check,"abc"}
    6> flush().
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"abc\" is not a palindrome."}
    ok
    7> Server ! stop.
    Server stopped.
    stop
    8> flush().
    ok
    9>

And some more after placing loop recursion where it belongs: at the end of
the function, not inside the case statement.

.. code:: erlang

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e1/palind", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palind}
    2> Self = self().
    <0.79.0>
    3> Server = spawn(palind, server, [Self]).
    <0.87.0>
    4> Server ! {check, "abc"}.
    {check,"abc"}
    5> Server ! {check, "def"}.
    {check,"def"}
    6> Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    7> Server ! {check, "Madam I\'m not Adam"}.
    {check,"Madam I'm not Adam"}
    8> flush().
    Shell got {result,"\"abc\" is not a palindrome."}
    Shell got {result,"\"def\" is not a palindrome."}
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"Madam I'm not Adam\" is not a palindrome."}
    ok
    9> Server ! {check, "oh! ho!"}.
    {check,"oh! ho!"}
    10> Server ! {check, "oh! no!"}.
    {check,"oh! no!"}
    11> flush().
    Shell got {result,"\"oh! ho!\" is not a palindrome."}
    Shell got {result,"\"oh! no!\" is not a palindrome."}
    ok
    12> Server ! {check, "Never odd or even"}.
    {check,"Never odd or even"}
    13> flush().
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
    {ok,palind}
    2> Server = spawn(palind, server, [self()]).
    <0.86.0>
    3> Server ! "invalidly formatted message".
    "invalidly formatted message"
    4> Server ! {check, "ahha"}.
    {check,"ahha"}
    5> Server ! {check, "never odd or even"}.
    {check,"never odd or even"}
    6> Server ! {check, "Madam I\'m Adam"}.
    {check,"Madam I'm Adam"}
    7> Server ! {check, "abc"}.
    {check,"abc"}
    8> flush().
    Shell got {result,"\"ahha\" is a palindrome"}
    Shell got {result,"\"never odd or even\" is a palindrome"}
    Shell got {result,"\"Madam I'm Adam\" is a palindrome"}
    Shell got {result,"\"abc\" is not a palindrome."}
    ok
    9> Server ! stop.
    Server stopped.
    stop
    10> Server ! {check, "abc"}.
    {check,"abc"}
    11> flush().
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
client and a server.

The client is `palindc.erl`_ exposes 4 functions: the start/0 and stop/1
which must be called to start the server and stop it, and the two
palindrome verification functions, is_palindrome/2 and
check_palindrome/2.  Their first argument is the server, and their second
is the  string to check.

This implementation does not hide the server process ID as I would have like
to do, but it hides the protocol from the user.  I would have liked to place
all protocol details inside one code location (one file), but that's not dome
here.


Here's a session using this code, with an Erlang shell running inside Emacs:

.. code:: erlang

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e1/palindc", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palindc}
    2> c("/Users/roup/doc/trying-erlang/exercises/e1/palinds", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e1/"}]).
    {ok,palinds}
    3> Server = palindc:start().
    <0.91.0>
    4> palindc:is_palindrome(Server, "never odd or even").
    true
    5> palindc:check_palindrome(Server, "never odd or even").
    {ok,"\"never odd or even\" is a palindrome"}
    6> palindc:check_palindrome(Server, "Madam, I'm Adam").
    {false,"\"Madam, I'm Adam\" is not a palindrome."}
    7> palindc:check_palindrome(Server, "Madam I'm Adam").
    {ok,"\"Madam I'm Adam\" is a palindrome"}
    8> palindc:check_palindrome(Server, "Madam I\'m Adam").
    {ok,"\"Madam I'm Adam\" is a palindrome"}
    9> palindc:check_palindrome(Server, "abc").
    {false,"\"abc\" is not a palindrome."}
    10> palindc:is_palindrome(Server, "abc").
    false
    11> palindc:stop().
    ** exception error: undefined function palindc:stop/0
    12> palindc:stop(Server).
    Palindrome checker server stopped.
    stop
    13> palindc:is_palindrome(Server, "never odd or even").
      C-c C-c
    BREAK: (a)bort (A)bort with dump (c)ontinue (p)roc info (i)nfo
           (l)oaded (v)ersion (k)ill (D)b-tables (d)istribution
    a

    Process inferior-erlang finished


Code Improvements
-----------------

After the first implementation I did the following changes:

- Renamed the variable `Client` to `From`.  Both are valid, but the second is
  shorter and seems to be used more often in Erlang.  In an environment where
  everything is a communication channel link, `From` is probably a little more
  flexible.
- There was nothing preventing some other process from sending some answers
  back to the client, so I modified the protocol between palindc_ and palinds_
  such that the Pid of the server is part of the reply message, allowing the
  client to discard messages received from some other processes.

And then yet another change: adding a timeout in the client in case the server
was stopped.  The new code for the 2 functions is now:

.. code:: erlang

    is_palindrome(Server, Text) ->
        Server ! {self(), check, Text},
        receive
            {Server, {is_a_palindrome, _}}  -> true;
            {Server, {not_a_palindrome, _}} -> false;
            _Other                          -> {error, _Other}
        after 1000 -> {timeout, Text}     %% <- new!
        end.

    check_palindrome(Server, Text) ->
        Server ! {self(), check, Text},
        receive
            {Server, {is_a_palindrome,  Report}} -> {ok, Report};
            {Server, {not_a_palindrome, Report}} -> {false, Report};
            _Other                               -> {error, _Other}
        after 1000 -> {timeout, Text}     %% <- new!
        end.

I would have liked to specify a timeout as a constant somewhere, used in both
functions instead of being hard coded, but that'll be for later.  At least
now, calling these functions when the server is stopped will no longer hang
the caller.

Here's a session using this new code:

.. code:: erlang

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> Server = palindc:start().
    <0.81.0>
    2> palindc:is_palindrome(Server, "abba").
    true
    3> palindc:check_palindrome(Server, "abba").
    {ok,"\"abba\" is a palindrome"}
    4> palindc:check_palindrome(Server, "abbacus").
    {false,"\"abbacus\" is not a palindrome."}
    5> palindc:stop(Server).
    Palindrome checker server stopped.
    stop
    6> palindc:check_palindrome(Server, "abbacus").
    {timeout,"abbacus"}
    7> palindc:is_palindrome(Server, "abba").
    {timeout,"abba"}
    8>

The calls at 6 and 7 are done while the server is stopped, so the returned
value indicates a timeout.

Now lets see what happens if I send an invalid message, not handled by the
code:

.. code:: erlang

    9> f(Server).
    ok
    10> Server = palindc:start().
    <0.92.0>
    11> palindc:is_palindrome(Server, 1.0).
    =ERROR REPORT==== 24-Jun-2020::12:02:10.566701 ===
    Error in process <0.92.0> with exit value:
    {function_clause,[{lists,'-filter/2-lc$^0/1-0-',
                             [1.0],
                             [{file,"lists.erl"},{line,1286}]},
                      {palinds,palindrome_check,1,
                               [{file,"/Users/roup/doc/trying-erlang/exercises/e1/palinds.erl"},
                                {line,34}]},
                      {palinds,loop,0,
                               [{file,"/Users/roup/doc/trying-erlang/exercises/e1/palinds.erl"},
                                {line,20}]}]}

    {timeout,1.0}
    12> palindc:is_palindrome(Server, "abba").
    {timeout,"abba"}
    13>

First I forget Server to be able to re-bind it.
Then I send it a float instead of a string.  That generates a dump trace: the
server crashed!  Then, without re-starting the server, I issue another
request, and then it times out, as expected.  Good.

Now the server, or the client, should reject invalid data.  That's for later.


More Improvements - Guard against invalid Input and Types
---------------------------------------------------------

The previous version accepted any input.  It was possible to pass a float
value instead of a string.  So I added a guard to check if the input is a
list. I would have liked to use a BIF predicate that checks for a string,
(something like `is_string`) but unfortunately Erlang does not support
something like that.

Then I added type specifications. For that, I first wanted to see if I could
run TypEr to infer the types and get me the first list. I ran typer from a
bash shell but that failed.

So I read the section titled
`Type Specifications and Erlang - PLTs Are The Best Sandwiches`_
from Fred Hébert's `Learn You Some Erlang for Great Good`_.  This explains
that you must first built Dialyzer's PLT (Persistent Lookup Table), so I did
and then typer worked fine.


.. _Learn You Some Erlang for Great Good: https://learnyousomeerlang.com
.. _Type Specifications and Erlang - PLTs Are The Best Sandwiches: https://learnyousomeerlang.com/dialyzer#plt


.. code:: shell

    >Pierres-iMac@Wed Jun 24@16:21:03[~/doc/trying-erlang/exercises/e1]
    > typer palinds.erl

    %% File: "palinds.erl"
    %% -------------------
    -spec loop() -> {'ok','stopped'}.
    -spec quoted(text()) -> text().
    -spec palindrome_check(text()) -> boolean().
    -spec to_small([any()]) -> text().
    -spec rem_punct(text()) -> text().
    >Pierres-iMac@Wed Jun 24@16:21:11[~/doc/trying-erlang/exercises/e1]
    > typer palindc.erl

    %% File: "palindc.erl"
    %% -------------------
    -spec start() -> pid().
    -spec stop(pid()) -> 'ok'.
    -spec is_palindrome(pid(),text()) -> boolean() | {'error',text()} | {'timeout',t
    -spec check_palindrome(pid(),text()) -> {'error',_} | {'false',text()} | {'ok',t
    {'timeout',text()}.
    >Pierres-iMac@Wed Jun 24@16:22:49[~/doc/trying-erlang/exercises/e1]
    >

I added something similar but also provided a type called ``text()`` that is a
list of ``char()``.

So , for instance the code for the two client functions now has a type
spec and a guard:

.. code:: erlang

    %% Types
    -type(text() :: [char()]).

    -spec is_palindrome(pid(), text()) ->
              boolean() | {'error', text()} | {'timeout',text()}.

    is_palindrome(Server, Text) when is_list(Text)  ->
        Server ! {self(), check, Text},
        receive
            {Server, {is_a_palindrome, _}}  -> true;
            {Server, {not_a_palindrome, _}} -> false;
            _Other                          -> {error, _Other}
        after 1000 -> {timeout, Text}
        end.


    -spec check_palindrome(pid(),text()) ->
              {'error',_} | {'false',text()} | {'ok',text()} | {'timeout',text()}.

    check_palindrome(Server, Text) when is_list(Text) ->
        Server ! {self(), check, Text},
        receive
            {Server, {is_a_palindrome,  Report}} -> {ok, Report};
            {Server, {not_a_palindrome, Report}} -> {false, Report};
            _Other                               -> {error, _Other}
        after 1000 -> {timeout, Text}
        end.

Trying to pass 1.0 to a function is intercepted right at the call, it does not
percolate up to the server to make it crash.  I also sent text that includes
non-ASCII characters:

.. code:: erlang::

    1> Server = palindc:start().
    <0.81.0>
    2> palindc:is_palindrome(1.0).
    ** exception error: undefined function palindc:is_palindrome/1
    3> palindc:is_palindrome(Server, 1.0).
    palindc:is_palindrome(Server, 1.0).
    ** exception error: no function clause matching palindc:is_palindrome(<0.81.0>,1.0) (/Users/roup/doc/trying-erlang/exercises/e1/palindc.erl, line 31)
    4> palindc:is_palindrome(Server, "abc").
    false
    5> palindc:is_palindrome(Server, "abba").
    true
    6> palindc:is_palindrome(Server, "a∫∫a").
    true
    7> palindc:is_palindrome(Server, "a∫ ΩΩ ∫a").
    true
    8> palindc:check_palindrome(Server, "a∫ ΩΩ ∫a").
    {ok,[34,97,8747,32,937,937,32,8747,97,34,32,105,115,32,97,
         32,112,97,108,105,110,100,114,111,109,101]}
    9> palindc:stop(Server).
    ok
    10>



Looking Back
------------

The user of this code must be aware that calling palindc:is_palindrome() and
palindc:check_palindrome() *must* be done while their server is running.
Otherwise, as shown after I stopped the server, their call just hang.

I' would have liked to find a way to detect that their server is not running
and if it was not these functions would spawn the server.  Ideally, the
functions would also have the ability to hold the PID of their server so the
user would not have to know about them.  That might not be the way of thinking
in Erlang.  I'm not sure.


Over time I have found that distribution of logic increases the probability of
making errors.  Using a build system that is able to detect mismatches in the
protocol also helps.  Back in the 90's I built a complete network management
system with it's own management protocol in C++ with an embedded pseudo mini
language using specialized comments and the C pre-processor.  The complete
protocol was based on binary data structure and types were known and checked
both statically and also dynamically at some gates in the system.  That made
creating data structure a little bit more painful because of the extra code
required to annotate the C data structures, but that really paid off.  Over 15
years of this system being deployed in the field we never had 1 bug detected
on protocol mismatch.

I'd like to be able to find a way to do this with a BEAM system.  At this
point I don't see how this can be done.  Hopefully I'll learn how to do it in
Erlang later in my readings and in this course.

.. _palindc:
.. _palindc.erl: palindc.erl
.. _palinds:
.. _palinds.erl: palinds.erl


..
   -----------------------------------------------------------------------------
