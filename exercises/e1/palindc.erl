%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking client to the palindrome server palinds.erl

-module(palindc).
-export([is_palindrome/2, check_palindrome/2, start/0, stop/1]).

%% - External server controls.

%% File: "palindc.erl"
%% -------------------

-spec start() -> pid().
start() -> spawn(palinds, loop, []).

-spec stop(pid()) -> 'ok'.
stop(Server) -> Server ! stop,
                ok.

%% - Palindrome verification functions

-spec is_palindrome(pid(), string()) ->
          boolean() | {'error', string()} | {'timeout',string()}.

is_palindrome(Server, Text) when is_list(Text)  ->
    Server ! {self(), check, Text},
    receive
        {Server, {is_a_palindrome, _}}  -> true;
        {Server, {not_a_palindrome, _}} -> false;
        _Other                          -> {error, _Other}
    after 1000 -> {timeout, Text}
    end.


-spec check_palindrome(pid(),string()) ->
          {'error',_} | {'false',string()} | {'ok',string()} | {'timeout',string()}.
check_palindrome(Server, Text) ->
    Server ! {self(), check, Text},
    receive
        {Server, {is_a_palindrome,  Report}} -> {ok, Report};
        {Server, {not_a_palindrome, Report}} -> {false, Report};
        _Other                               -> {error, _Other}
    after 1000 -> {timeout, Text}
    end.

%% -----------------------------------------------------------------------------
