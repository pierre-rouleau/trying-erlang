%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking server to its client palindc2.erl

-module(palinds2).
-export([loop/0]).

%% - Server process loop.

-spec loop() -> {'ok','stopped'}.
loop() ->
    receive
        {From, stop} ->
            io:format("Palindrome checker server stopped.~n"),
            From ! {ok, stopped};
        {From, check, Text} ->
            case palindrome:check(Text) of
                true  -> From ! {self(), {is_a_palindrome,  quoted(Text) ++ " is a palindrome"}};
                false -> From ! {self(), {not_a_palindrome, quoted(Text) ++ " is not a palindrome."}}
            end,
            loop();
        _Other  -> loop()
    end.


-spec quoted(string()) -> string().
quoted(Text) -> "\"" ++ Text ++ "\"".

%% -----------------------------------------------------------------------------

% LocalWords:  palindc2 erl
