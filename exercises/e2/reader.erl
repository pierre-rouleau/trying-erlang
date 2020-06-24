%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 2 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488337

%% Author: Pierre Rouleau.

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


%% -----------------------------------------------------------------------------
