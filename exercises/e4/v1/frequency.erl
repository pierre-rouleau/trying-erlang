%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise  : https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488342
%%%  - Enhancing the frequency server with a functional interface
%%%
%%% Last Modified Time-stamp: <2020-07-08 22:08:15, updated by Pierre Rouleau>
%% -----------------------------------------------------------------------------

%% What's New
%% ----------
%% - Providing a functional interface to the requests:
%%   - allocate()
%%   - deallocate(Freq)
%%   - dump()
%%   - stop()
%%

%% Supported Transactions
%% ----------------------
%%
%% Here's the representation of the supported transactions:
%%
%% @startuml
%%
%% actor Client
%% boundary API
%% database Frequency
%%
%% == Operation: start the server ==
%% Client ->o API : start()
%% API    o-->o Frequency : register(spawn())
%% Client <-o API : ok | {error, Error}
%%
%% == Operation: successful allocation ==
%%
%% Client ->o API : allocate()
%% API --> Frequency : {request, Pid, allocate}
%% API <-- Frequency : {reply, {ok, Freq}}
%% Client <-o API : {ok, Freq}
%%
%% == Operation: successful de-allocation ==
%%
%% Client ->o API: deallocate(Freq)
%% API    --> Frequency : {request, Pid, {deallocate, Freq}}
%% API    <-- Frequency : {reply, ok}
%% Client <-o  API : ok
%%
%%
%% == Error: failed allocation (no available frequency) ==
%%
%% Client ->o API : allocate()
%% API    --> Frequency : {request, Pid, allocate}
%% API    <-- Frequency : {reply, {error, no_frequency}}
%% Client <-o API : {error, no_frequency}
%%
%% == Error: failed allocation (client already owns one) ==
%%
%% Client ->o API : allocate()
%% API --> Frequency : {request, Pid, allocate}
%% API <-- Frequency : {reply, {error, client_already_owns, Freq}}
%% Client <-o API : {error, client_already_owns, Freq}
%%
%% == Error: failed de-allocation (frequency not allocated by client) ==
%%
%% Client ->o API : deallocate(Freq)
%% API --> Frequency : {request, Pid, {deallocate, Freq}}
%% API <-- Frequency : {reply, {error, client_does_not_own, Freq}}
%% Client <-o  API : {error, client_does_not_own, Freq}
%%
%% == Development help ==
%%
%% Client ->o API : dump()
%% API --> Frequency : {request, Pid, dump}
%% API <-- Frequency : {reply, FreqDb}
%% Client <-o API : FreqDb
%%
%% == Shutdown ==
%%
%% Client ->o API: stop()
%% API --> Frequency : {request, Pid, stop}
%% API <-- Frequency : {reply, stopped}
%% Client <-o API : stopped
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
-export([start/0, init/0, allocate/0, deallocate/1, dump/0, stop/0]).

%% Data Model:
%%    FreqDb := {free:[integer], allocated:[{integer, pid}]}


%%% Public API

%% start/0 : start the server
%%  return : ok | {error, Error}
start() ->
    case register(frequency, spawn(frequency, init, [])) of
        true ->  ok;
        Error -> {error, Error}
    end.

%% allocate/0 : allocate a frequency for the caller's process
%%     return :  {ok, Freq} | {error, client_already_own, Freq{}
allocate() ->
    frequency ! {request, self(), allocate},
    receive {reply, Reply} ->
             Reply
    end.

%% deallocate/1 : deallocate a specified frequency that should have
%%                already have been allocated by the caller's process.
%%       return : ok | {error, client_does_not_own, Freq}
deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive {reply, Reply} ->
            Reply
    end.

%% dump/0 : return internal database data (should really be debug only)
dump() ->
    frequency ! {request, self(), dump},
    receive {reply, FreqDb} ->
            FreqDb
    end.

% stop/0 : stop the frequency server
stop() ->
    frequency ! {request, self(), stop},
    receive {reply, Reply} ->
            Reply
    end.


%%% Internal process logic

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
