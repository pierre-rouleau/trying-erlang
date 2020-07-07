%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise  : https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488342
%%%  - Version 1 (slides version): explicit send & receive, limited error handling.
%%%
%%% Last Modified Time-stamp: <2020-07-07 15:25:13, updated by Pierre Rouleau>

%% This file implements a naive frequency server where the client requests
%% allocation and de-allocation of frequencies.
%%
%% Here's the representation of the exchanges supported:
%%
%%

%%
%% The server functional state is:
%% - a pair of lists {Free, Allocated}
%%   - Free := a list of integers
%%   - Allocated: a list of {Freq, UserPid}
%% - the allocate/2 function to request a frequency allocation
%% - the deallocate/2 function to de-allocate previously allocated frequency.
%%
%% Supported Interaction
%% =====================
%% @startuml
%%
%% actor Client
%% database Frequencies
%%
%% == Initialization: explicit spawn ==
%%
%% Client -> Frequencies : spawn(frequencies, init, [])
%%
%%
%% == Operation: successful allocation ==
%%
%% Client -> Frequencies : {request, Pid, allocate}
%% Client <-- Frequencies : {reply, {ok, Freq}}
%%
%% == Operation: failed allocation (no available frequency) ==
%%
%% Client -> Frequencies : {request, Pid, allocate}
%% Client <-- Frequencies : {reply, {error, no_frequency}}
%%
%% == Operation: successful de-allocation ==
%%
%% Client -> Frequencies : {request, Pid, {deallocate, Freq}}
%% Client <-- Frequencies : {reply, ok}
%%
%%
%% == Development help ==
%%
%% Client -> Frequencies : {request, Pid, dump}
%% Client <-- Frequencies : {reply, FreqDb}
%%
%% == Shutdown ==
%%
%% Client -> Frequencies : {request, Pid, stop}
%% Client <- Frequencies : {reply, stopped}
%%
%% @enduml



-module(frequencies).
-export([init/0, allocate/2, deallocate/2]).

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
            NewFreqDb = deallocate(FreqDb, Freq),
            Pid! {reply, ok},
            loop(NewFreqDb);
        {request, Pid, dump} ->
            Pid! {reply, FreqDb},
            loop(FreqDb);
        {request, Pid, stop} ->
            Pid! {reply, stopped}
    end.


%% Frequency 'Database' management functions.
%% TODO: each function should return the same {FreqDb, Result}

%% allocate:
%%   1) when all frequencies are allocated (none free)
allocate({[], Allocated}, _Pid) ->
    { {[], Allocated},
      {error, no_frequency} };
%%   2) when some frequency free
allocate({[Freq|Free], Allocated}, Pid) ->
    { {Free, [{Freq, Pid} | Allocated]},
      {ok, Freq} }.

%% deallocate:
deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
%% TODO: deal with case where Freq is already free.


%% Database initialization

get_frequencies() ->
    [10,11,12,13,14,15].

%% -----------------------------------------------------------------------------
