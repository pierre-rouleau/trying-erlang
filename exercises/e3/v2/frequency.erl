%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise  : https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488342
%%%  - Version 2 : Named (registered) server that is less permissive.  Renamed to frequency.erl.
%%%
%%% Last Modified Time-stamp: <2020-07-08 12:04:45, updated by Pierre Rouleau>
%% -----------------------------------------------------------------------------

%% What's New
%% ----------
%% A better server that builds on v1 and adds:
%%
%% - Prevents allocation of multiple frequencies by a client,
%% - Prevents de-allocation of a frequency not allocated by the requester,
%% - Prevents de-allocation of a currently free frequency (note that the previous
%%   requirement handles this one).
%%

%% Supported Transactions
%% ----------------------
%%
%% Here's the representation of the supported transactions:
%%
%% @startuml
%%
%% actor Client
%% database Frequency
%%
%% == Initialization: explicit spawn ==
%%
%% Client -> Frequency : spawn(frequencies, init, [])
%%
%%
%% == Operation: successful allocation ==
%%
%% Client -> Frequency : {request, Pid, allocate}
%% Client <-- Frequency : {reply, {ok, Freq}}
%%
%% == Operation: successful de-allocation ==
%%
%% Client -> Frequency : {request, Pid, {deallocate, Freq}}
%% Client <-- Frequency : {reply, ok}
%%
%%
%%
%% == Error: failed allocation (no available frequency) ==
%%
%% Client -> Frequency : {request, Pid, allocate}
%% Client <-- Frequency : {reply, {error, no_frequency}}
%%
%% == Error: failed allocation (client already owns one) ==
%%
%% Client -> Frequency : {request, Pid, allocate}
%% Client <-- Frequency : {reply, {error, client_already_owns, Freq}}
%%
%% == Error: failed de-allocation (frequency not allocated by client) ==
%%
%% Client -> Frequency : {request, Pid, {deallocate, Freq}}
%% Client <-- Frequency : {reply, {error, client_does_not_own, Freq}}
%%
%%
%% == Development help ==
%%
%% Client -> Frequency : {request, Pid, dump}
%% Client <-- Frequency : {reply, FreqDb}
%%
%% == Shutdown ==
%%
%% Client -> Frequency : {request, Pid, stop}
%% Client <- Frequency : {reply, stopped}
%%
%% @enduml

%% Server Functional State / Data Model
%% ------------------------------------
%% The server functional state is:
%% - a pair of lists {Free, Allocated}
%%   - Free := a list of frequency integers
%%   - Allocated: a list of {Freq, UserPid}
%%
%% Db access functions:
%% - allocate/2   : Allocate any frequency  for Client
%% - deallocate/3 : de-allocate client owned frequency
%%   - is_owner/2 : predicate: return {true, Freq} if Client owns a frequency,
%%                  False otherwise.
%%   - owns/3     : predicate: return true if Client owns a specific frequency.


-module(frequency).
-export([init/0, allocate/2, deallocate/3]).

%% Data Model:
%%    FreqDb := {free:[integer], allocated:[{integer, pid}]}

%% Usage: explicit spawn from client.

init() ->
    FreqDb = {get_frequencies(), []},
    loop(FreqDb).

loop(FreqDb) ->
    receive
        {request, Pid, allocate} ->
            {NewFreqDb, Result} = allocate(FreqDb, Pid),
            Pid ! {reply, Result},
            loop(NewFreqDb);
        {request, Pid, {deallocate, Freq}}  ->
            {NewFreqDb, Result} = deallocate(FreqDb, Freq, Pid),
            Pid! {reply, Result},
            loop(NewFreqDb);
        {request, Pid, dump} ->
            Pid! {reply, FreqDb},
            loop(FreqDb);
        {request, Pid, stop} ->
            Pid! {reply, stopped}
    end.


%% Frequency 'Database' management functions.

%% allocate/2: FreqDb, ClientPid
%% allocate a frequency for ClientPid.  Allow 1 frequency per Client.
%% Return:  {FreqDb, Reply}
%%   1) when all frequencies are allocated (none free)
allocate({[], Allocated}, _Pid) ->
    { {[], Allocated},
      {error, no_frequency} };
%%   2) with some available frequency/ies
allocate({[Freq|Free], Allocated}, Pid) ->
    case is_owner(Allocated, Pid) of
        false ->    { {Free, [{Freq, Pid} | Allocated]},
                      {ok, Freq} };
        {true, OwnedFreq} -> { {[Freq|Free], Allocated},
                               {error, client_already_owns, OwnedFreq} }
    end.

%% deallocate/3 : FreqDb, Freq, Pid
%% de-allocate client owned frequency
%% Return:  {FreqDb, Reply}
deallocate({Free, Allocated}, Freq, Pid) ->
    case owns(Allocated, Freq, Pid) of
        true ->     NewAllocated = lists:keydelete(Freq, 1, Allocated),
                    { {[Freq|Free], NewAllocated},
                      ok };
        false ->    { {Free, Allocated},
                      {error, client_does_not_own, Freq} }
    end.

%%% Database verifications

%% is_owner/2 : Allocated, ClientPid
%% Return {true, Freq} when ClientPid already owns a frequency, false otherwise.
is_owner([], _ClientPid) -> false;
is_owner([{Freq, ClientPid} | _AllocatedTail], ClientPid) -> {true, Freq};
is_owner([_Head | Tail], ClientPid) -> is_owner(Tail, ClientPid).

%% owns/3 : Allocated, Freq, ClientPid
%% Return true when ClientPid owns Freq, false otherwise.
owns([], _Freq, _ClientPid) -> false;
owns([{Freq, ClientPid} | _AllocatedTail], Freq, ClientPid) -> true;
owns([_Head | Tail], Freq, ClientPid) -> owns(Tail, Freq, ClientPid).


%%% Database initialization

get_frequencies() ->
    [10,11,12,13,14,15].

%% -----------------------------------------------------------------------------
