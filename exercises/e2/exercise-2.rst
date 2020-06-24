=====================================
Exercise 2 - Working with the mailbox
=====================================

- `Exercise 2`_
- Code: `reader.erl`_

.. _Exercise 2: https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488337


.. contents::  **Table Of Contents**
.. sectnum::


A Receiver that can sleep on request
====================================

I wrote a receiver process that can be put to sleep for 5, 10 or 30 seconds.
It also handles a special message, `priority`, which starts receiving only
floating point and integer values, leaving everything else in the mailbox.
To exit that state, the client must send the `normal` message.

This helps put the receiver in a state where it won't accept `stop` messages
and won't process atoms, lists, tuples.  So we can see them accumulate or
being dropped if we send a `stop` and then other non-prioritized message while
the receiver is only processing the *priority* number messages.

To avoid having to repeat the recursive call to each reception clause, I
wrote secondary functions that handle the work and do the recursive calling.

The code is in reader.erl_, and is reproduced here:

.. code:: erlang

    -module(reader).
    -export([receiver/0]).


    %% Using indirect recursion to avoid repeating the call back to receiver in
    %% each receiver receive clause that needs to continue receiving.
    %% The stop message has the highest priority (via its precedence in the clauses).
    %% To make the reader sleep, send a 'short_wait', 'wait' or 'long_wait'
    %% message to it.  They are also the next on the priority order in the order
    %% given, so we can schedule a short wait in between longer waits.
    %% A short wait is special, it also reads *one* message, so we can schedule a
    %% long wait, then a short wait, then several messages, and only 1 of those
    %% will be read and echoed before the reader sleeps again as long as there
    %% were no stop message(s).


    receiver() ->
        receive
            stop ->
                io:format("Stopping, bye!~n"),
                ok;
            priority   -> io:format("You have 30 seconds to send your prioritized messages!~n"),
                          timer:sleep(30000),
                          receiver2(),
                          wait(2);
            short_wait -> wait(5);
            wait       -> wait(10);
            long_wait  -> wait(30);
            Msg ->        process(Msg)
        end.

    receiver2() ->
        receive
            Value when is_float(Value) ->
                io:format("Got (prioritized): ~w~n", [Value]),
                receiver2();
            Value when is_integer(Value)  ->
                io:format("Got (prioritized): ~w~n", [Value]),
                receiver2();
            normal ->
                io:format("Back to normal reception~n")
        end.



    wait(Seconds) ->
        io:format("Received sleep request! Sleeping for ~w seconds...~n", [Seconds]),
        timer:sleep(Seconds * 1000),
        io:format("Back listening!~n"),
        receiver().

    process(Msg) ->
        io:format("Got: ~w~n", [Msg]),
        receiver().




.. _reader.erl: reader.erl


Here's a session using that mechanism and ending up sending a message when the
reader has already stopped (step 20):

.. code:: erlang

    Erlang/OTP 22 [erts-10.7.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

    Eshell V10.7.2  (abort with ^G)
    1> c("/Users/roup/doc/trying-erlang/exercises/e2/reader", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e2/"}]).
    c("/Users/roup/doc/trying-erlang/exercises/e2/reader", [{outdir, "/Users/roup/doc/trying-erlang/exercises/e2/"}]).
    {ok,reader}
    2> S = spawn(reader, receiver, []).
    S = spawn(reader, receiver, []).
    <0.86.0>
    3> S ! [1,2,3].
    S ! [1,2,3].
    Got: [1,2,3]
    [1,2,3]
    4> S ! "Hello".
    S ! "Hello".
    Got: [72,101,108,108,111]
    "Hello"
    5> S ! 123.
    S ! 123.
    Got: 123
    123
    6> S ! wait.
    S ! wait.
    Received sleep request! Sleeping for 10 seconds...
    wait
    7> S ! 1.
    S ! 1.
    1
    8> S ! 3.
    S ! 3.
    3
    Back listening!
    Got: 1
    Got: 3
    9> S ! priority.
    S ! priority.
    You have 30 seconds to send your prioritized messages!
    priority
    10> S ! {a,b}.
    S ! {a,b}.
    {a,b}
    11> S ! 3.4.
    S ! 3.4.
    3.4
    12> S ! 3.
    S ! 3.
    3
    13> S ! a.
    S ! a.
    a
    Got (prioritized): 3.4
    Got (prioritized): 3
    14> S ! [ 1,2,3,4].
    S ! [ 1,2,3,4].
    [1,2,3,4]
    15> S ! stop.
    S ! stop.
    stop
    16> S ! 3.3.
    S ! 3.3.
    Got (prioritized): 3.3
    3.3
    17> S ! 123.
    S ! 123.
    Got (prioritized): 123
    123
    18> S ! "will never be processed".
    S ! "will never be processed".
    "will never be processed"
    19> S ! normal.
    S ! normal.
    Back to normal reception
    normal
    Received sleep request! Sleeping for 2 seconds...
    Back listening!
    Got: {a,b}
    Got: a
    Got: [1,2,3,4]
    Stopping, bye!
    20> S ! 'is there anyone in there?".
    S ! 'is there anyone in there?".
    20> flush().
    flush().
    20>

-------------------------------------------------------------------------------
