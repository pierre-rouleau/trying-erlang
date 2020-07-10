%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise  : https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488342
%%%  v2 - += Flushing the mailbox, adding timeout to client code
%%%
%%% Last Modified Time-stamp: <2020-07-10 10:57:29, updated by Pierre Rouleau>
%% -----------------------------------------------------------------------------

%% What's New
%% ----------
%% - v2.1: - Fixed a bug in loop patter for set_wait: A *new* variable must be
%%           used for the time: ``NewWaitTime`` otherwise it patterns match
%%           only if the wait time value does *not* change!
%%         - Placed clear() code close to where it's used.
%%         - Added several io:format to see the clear and delay activities.
%% - v2: instrument for simulating server loading:
%%       - client can now timeout after CLIENT_RX_TIMEOUT (set to 1 second via a macro)
%%       - Data structure change: FreDb has a TestData field.
%%         For now it holds a tuple of 1 tagged value: {sleep_period, integer}
%%         identifying the time the server should sleep before each receive
%%         to let message accumulate in its mailbox.
%%       - Added new debug command/message: set_server_load/1 which identifies
%%         how long the server should sleep.
%%       - Added clear/0 which clears a mailbox, printing each message removed
%%         and returning the number of cleared message.
%%         It is called by the client before the client sends a new request,
%%         to flush previous un-processed replies.
%% - v1: Providing a functional interface to the requests:
%%       - allocate()
%%       - deallocate(Freq)
%%       - dump()
%%       - stop()
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
%% == Timeout: *for any command*: timeout waiting for server reply ==
%%
%% Client -> API : allocate() | deallocate(Freq) | dump() | set_server_load(WaitTime)
%% API  -->x Frequency : {request, Pid, Msg}
%% Client <- API : {error, timeout}
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
%% == Development help: dump DB ==
%%
%% Client ->o API : dump()
%% API --> Frequency : {request, Pid, dump}
%% API <-- Frequency : {reply, FreqDb}
%% Client <-o API : FreqDb
%%
%% == Development help: set server load ==
%%
%% Client ->o API : set_server_load(WaitTime)
%% API --> Frequency : {request, Pid, {set_wait, WaitTime}}
%% API <-- Frequency : {reply, {ok, OldWaitTime}}
%% Client <-o API : {ok, OldWaitTime}
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
-export([start/0, init/0, allocate/0, deallocate/1, dump/0, set_server_load/1,  stop/0]).

%% Data Model:
%%    FreqDb := { free     : [integer],
%%                allocated: [{integer, pid}]
%%                test     : sleep_period := integer
%%               }


%%% Public API
-define(CLIENT_RX_TIMEOUT, 3000).   % Timeout for client waiting for server reply.

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
    Cleared = clear(),
    io:format("set_server_load(): cleared: ~w~n", [Cleared]),
    frequency ! {request, self(), allocate},
    receive {reply, Reply} ->
             Reply
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

%% deallocate/1 : deallocate a specified frequency that should have
%%                already have been allocated by the caller's process.
%%       return : ok | {error, client_does_not_own, Freq}
deallocate(Freq) ->
    Cleared = clear(),
    io:format("set_server_load(): cleared: ~w~n", [Cleared]),
    frequency ! {request, self(), {deallocate, Freq}},
    receive {reply, Reply} ->
            Reply
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

%% dump/0 : return internal database data (should really be debug only)
dump() ->
    Cleared = clear(),
    io:format("set_server_load(): cleared: ~w~n", [Cleared]),
    frequency ! {request, self(), dump},
    receive {reply, FreqDb} ->
            FreqDb
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

%% set_server_load/1 : WaitTime (in milliseconds)
%% Return: ok | {error, timeout}
set_server_load(WaitTime) ->
    io:format("set_server_load()~n"),
    Cleared = clear(),
    io:format("set_server_load(): cleared: ~w~n", [Cleared]),
    frequency ! {request, self(), {set_wait, WaitTime}},
    io:format("set_server_load(): request sent, waiting for reply~n"),
    receive {reply, Reply} ->
            Reply
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

% stop/0 : stop the frequency server
stop() ->
    clear(),
    frequency ! {request, self(), stop},
    receive {reply, Reply} ->
            Reply
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

%%% Client API utility function

%% clear/0: clear the mailbox
%%   return: number of cleared messages.
%%   side effect: prints each cleared message on stdout.

clear() -> clear(0).
clear(ClearCount) ->
    receive
        Msg ->
            io:format("Cleared Message: ~w~n", [Msg]),
            clear(ClearCount + 1)
    after 0 -> {ok, ClearCount}
    end.

%% -----------------------------------------------------------------------------
%%% Server - Internal process logic

init() ->
    FreqDb = {get_frequencies(), [], {sleep_period, 0}},
    loop(FreqDb).

loop(FreqDb) ->
    {_Allocated, _Free, {sleep_period, WaitTime}} = FreqDb,
    io:format("loop: waiting ~w...~n", [WaitTime]),
    timer:sleep(WaitTime),
    io:format("loop: receiving~n"),
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
        {request, Pid, {set_wait, NewWaitTime}} ->
            io:format("loop received set_wait(~w)~n", [NewWaitTime]),
            {NewFreqDb, Result} = set_wait(FreqDb, NewWaitTime),
            Pid ! {reply, Result},
            loop(NewFreqDb);
        {request, Pid, stop} ->
            Pid! {reply, stopped};
        Msg  ->
            io:format("loop: rx unexpected: ~w~n", [Msg]),
            loop(FreqDb)
    end.


%% Frequency 'Database' management functions.

%% allocate/2: FreqDb, ClientPid
%% allocate a frequency for ClientPid.  Allow 1 frequency per Client.
%% Return:  {FreqDb, Reply}
%%   1) when all frequencies are allocated (none free)
allocate({[], Allocated, TestData}, _Pid) ->
    { {[], Allocated, TestData},
      {error, no_frequency} };
%%   2) with some available frequency/ies
allocate({[Freq|Free], Allocated, TestData}, Pid) ->
    case is_owner(Allocated, Pid) of
        false ->    { {Free, [{Freq, Pid} | Allocated], TestData},
                      {ok, Freq} };
        {true, OwnedFreq} -> { {[Freq|Free], Allocated, TestData},
                               {error, client_already_owns, OwnedFreq} }
    end.

%% deallocate/3 : FreqDb, Freq, Pid
%% de-allocate client owned frequency
%% Return:  {FreqDb, Reply}
deallocate({Free, Allocated, TestData}, Freq, Pid) ->
    case owns(Allocated, Freq, Pid) of
        true ->     NewAllocated = lists:keydelete(Freq, 1, Allocated),
                    { {[Freq|Free], NewAllocated, TestData},
                      ok };
        false ->    { {Free, Allocated, TestData},
                      {error, client_does_not_own, Freq} }
    end.

%% set_wait/2: FreqDb, WaitTime
%% set server sleep time to WaitTime
%% Return: {FreqDb, {ok, OldWaitTime}}
set_wait({Free, Allocated, {sleep_period, OldWaitTime}}, WaitTime) ->
    {{Free, Allocated, {sleep_period, WaitTime}}, {ok, OldWaitTime}}.



%%% Database verification

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
