%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise 1.5 : URL https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488334

%% Author: Pierre Rouleau.

%% Notes:  A very basic solution to the exercise.
%%
%% The server is passed the PID of its *unique* client and can therefore only
%% handle the requests of that client.  This does not make a powerful and
%% versatile server, but it answers the basic requirements.  This also helps
%% test the shell behaviour that replies get accumulated to the shell's
%% process mailbox and can be flushed with the flush/0 function to see the
%% replies.
%%
%% One interesting aspect is the fact that also shows that the order of
%% declarations is not significant.  No forward declarations are required and
%% it's possible to order function definitions order resembling the logic
%% hierarchy.
%%
%% Yet another aspect to investigate is the quoting conventions as far as
%% single and double quote is concerned. Double quotes delimit the strings,
%% so why is the single quote escaped in the exercise string and why does the
%% backslash disappears in the echo of the received message?
%% I tried to explicitly replace it with regexp substitution but that did not work.
%% More investigation/reading is needed here.

-module(palind).
-export([server/1]).

%% -- Basic server

server(Pid) ->
    receive
        stop ->
            io:format("Server stopped.~n");
        {check, Text} ->
            case palindrome_check(Text) of
                true  -> Pid ! {result, quoted(Text) ++ " is a palindrome"};
                false -> Pid ! {result, quoted(Text) ++ " is not a palindrome."}
            end
    end,
    server(Pid).

%% -- Base logic

%% The following does not attempt to escape single or double quotes.
%% This does not seem to do the trick::  re:replace(Text, "\'", "\\\'", [{return,list}])
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
