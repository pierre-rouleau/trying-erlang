%%%  Concurrent Programming In Erlang -- The University of Kent / FutureLearn
%%%  Exercise  : https://www.futurelearn.com/courses/concurrent-programming-erlang/3/steps/488342
%%%  v4 - += Compressed v3 code.
%%%
%%% Last Modified Time-stamp: <2020-07-18 11:12:07, updated by Pierre Rouleau>
%% -----------------------------------------------------------------------------

%% What's New
%% ----------
%% - v4.0: refactoring: common code placed in utility functions: call()
%%         is now used by all the functional interface.  Doing this identified
%%         *copy/paste* bugs.

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
-export([ start/0
        , allocate/0
        , deallocate/1
        , stop/0]).

-export([ dump/0
        , set_server_load/1
        , show_mailbox/0
        , show_mailbox/1]).

-export([init/0]).


%% -----------------------------------------------------------------------------
%% Data Model:
%%    FreqDb := { free     : [integer],
%%                allocated: [{integer, pid}]
%%                test     : sleep_period := integer
%%               }

-define(CLIENT_RX_TIMEOUT, 3000).   % Timeout for client waiting for server reply.

%% -----------------------------------------------------------------------------
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
allocate() -> call(frequency, allocate).

%% deallocate/1 : deallocate a specified frequency that should have
%%                already have been allocated by the caller's process.
%%       return : ok | {error, client_does_not_own, Freq}
deallocate(Freq) -> call(frequency, {deallocate, Freq}).

%%% Debugging Public API

%% dump/0 : return internal database data (should really be debug only)
dump() -> call(frequency, dump).

%% set_server_load/1 : WaitTime (in milliseconds)
%% Return: ok | {error, timeout}
set_server_load(WaitTime) -> call(frequency, {set_wait, WaitTime}).

% stop/0 : stop the frequency server
stop() -> call(frequency, stop).


%%% Client API utility function

%% call/2: send message and receive reply
%% return: reply
call(Server, Msg) ->
    Cleared = clear(),
    io:format("Before sending ~w, cleared: ~w~n", [Msg, Cleared]),
    send_request(Server, Msg),
    receive {reply, Reply} ->
             Reply
    after ?CLIENT_RX_TIMEOUT -> {error, timeout}
    end.

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

%% send_request/2: send message to server
send_request(Server, Msg) -> Server ! {request, self(), Msg}.

%% -----------------------------------------------------------------------------
%%% Server - Internal process logic

init() ->
    FreqDb = {get_frequencies(), [], {sleep_period, 0}},
    loop(FreqDb).

loop(FreqDb) ->
    %% extract WaitTime
    {_Allocated, _Free, {sleep_period, WaitTime}} = FreqDb,
    %% clear the mailbox
    Cleared = clear(),
    io:format("frequency loop(): cleared: ~w~n", [Cleared]),
    %% simulate a server load
    timer:sleep(WaitTime),
    %% normal processing
    receive
        {request, Pid, allocate} ->
            {NewFreqDb, Result} = allocate(FreqDb, Pid),
            send_reply(Pid, Result),
            loop(NewFreqDb);
        {request, Pid, {deallocate, Freq}}  ->
            {NewFreqDb, Result} = deallocate(FreqDb, Freq, Pid),
            send_reply(Pid, Result),
            loop(NewFreqDb);
        {request, Pid, dump} ->
            send_reply(Pid, FreqDb),
            loop(FreqDb);
        {request, Pid, {set_wait, NewWaitTime}} ->
            {NewFreqDb, Result} = set_wait(FreqDb, NewWaitTime),
            send_reply(Pid, Result),
            loop(NewFreqDb);
        {request, Pid, stop} ->
            send_reply(Pid, stopped)
    end.


%%% Server utility functions

send_reply(Pid, Result) -> Pid ! {reply, Result}.

%% -----------------------------------------------------------------------------
%%% Frequency 'Database' management functions.

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


%% show_mailbox/0 : print and return process mailbox size on stdout
show_mailbox() ->
    show_mailbox(self()).

%% show_mailbox/1 : print and return process mailbox size on stdout
show_mailbox(Pid) ->
    {message_queue_len, MsgCount} = process_info(Pid, message_queue_len),
    io:format("Size of ~w mailbox: ~w~n", [self(), MsgCount]),
    MsgCount.

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
