%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking server to its client palindc.erl

-module(palinds).
-export([loop/0]).

%% - Server process loop.

-spec loop() -> {'ok','stopped'}.
loop() ->
    receive
        {From, stop} ->
            io:format("Palindrome checker server stopped.~n"),
            From ! {ok, stopped};
        {From, check, Text} ->
            case palindrome_check(Text) of
                true  -> From ! {self(), {is_a_palindrome,  quoted(Text) ++ " is a palindrome"}};
                false -> From ! {self(), {not_a_palindrome, quoted(Text) ++ " is not a palindrome."}}
            end,
            loop();
        _Other  -> loop()
    end.


%% -- Base logic

-spec quoted(string()) -> string().
quoted(Text) -> "\"" ++ Text ++ "\"".


-spec palindrome_check(string()) -> boolean().
palindrome_check(String) ->
    Normalised = to_small(rem_punct(String)),
    lists:reverse(Normalised) == Normalised.

-spec to_small([any()]) -> string().
to_small(String) -> lists:map(fun(Ch) ->
                                      case ($A =< Ch andalso Ch =< $Z) of
                                          true -> Ch+32;
                                          false -> Ch
                                      end
                              end,
                              String).

-spec rem_punct(string()) -> string().
rem_punct(String) -> lists:filter(fun (Ch) ->
                                          not(lists:member(Ch,"\"\'\t\n "))
                                  end,
                                  String).

%% -----------------------------------------------------------------------------
