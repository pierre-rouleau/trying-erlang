%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking client to the palindrome server palinds.erl

-module(palindc).
-export([is_palindrome/2, check_palindrome/2, start/0, stop/1]).

%% - External server controls.

start() -> spawn(palinds, loop, []).

stop(Server) -> Server ! stop.

%% - Palindrome verification functions

is_palindrome(Server, Text) ->
    Server ! {self(), check, Text},
    receive
        {Server, {is_a_palindrome, _}}  -> true;
        {Server, {not_a_palindrome, _}} -> false;
        _Other                          -> {error, _Other}
    end.

check_palindrome(Server, Text) ->
    Server ! {self(), check, Text},
    receive
        {Server, {is_a_palindrome,  Report}} -> {ok, Report};
        {Server, {not_a_palindrome, Report}} -> {false, Report};
        _Other                               -> {error, _Other}

    end.

%% -----------------------------------------------------------------------------
