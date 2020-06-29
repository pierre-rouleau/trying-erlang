%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A palindrome checking logic.  Isolating the logic


-module(palindrome).
-export([check/1]).


%% --

-spec check(string()) -> boolean().
check(String) ->
    Normalised = to_small(rem_punct(String)),
    lists:reverse(Normalised) == Normalised.

%% -- Base logic

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
