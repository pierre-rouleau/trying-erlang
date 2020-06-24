%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking server to its client palindc.erl

-module(palinds).
-export([loop/0]).

%% - Server process loop.

loop() ->
    receive
        stop ->
            io:format("Palindrome checker server stopped.~n");
        {From, stop} ->
            From ! {ok, stopped};
        {From, check, Text} ->
            case palindrome_check(Text) of
                true  -> From ! {self(), {is_a_palindrome, quoted(Text) ++ " is a palindrome"}};
                false -> From ! {self(), {not_a_palindrome, quoted(Text) ++ " is not a palindrome."}}
            end,
            loop();
        _Other  -> loop()
    end.


%% -- Base logic

quoted(Text) -> "\"" ++ Text ++ "\"".

palindrome_check(String) ->
    Normalised = to_small(rem_punct(String)),
    lists:reverse(Normalised) == Normalised.

to_small(String) -> lists:map(fun(Ch) ->
                                      case ($A =< Ch andalso Ch =< $Z) of
                                          true -> Ch+32;
                                          false -> Ch
                                      end
                              end,
                              String).

rem_punct(String) -> lists:filter(fun (Ch) ->
                                          not(lists:member(Ch,"\"\'\t\n "))
                                  end,
                                  String).

%% -----------------------------------------------------------------------------
