-module(lmetrics).

-include("lmetrics.hrl").

-behaviour(gen_server).

%% lmetrics callbacks
-export([start_link/0,
         set_memory_callback/1,
         get_memory/0,
         stop_scheduling/0,
         record_transmission/2,
         get_transmission/0,
         record_latency/2,
         get_latency/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type metric() :: non_neg_integer().
-type memory() :: list({timestamp(), {metric(), metric()}}).

-type latency_type() :: local | remote.
-type latency() :: dict:dict(latency_type(), list({timestamp(), metric()})).

-type transmission_type() :: term().
-type transmission() :: dict:dict(transmission_type(),
    list({timestamp(), metric()})).

-record(state, {memory_callback :: function(),
                memory :: memory(),
                transmission :: transmission(),
                latency :: latency(),
                keep_scheduling :: atom()}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_memory_callback(function()) -> ok.
set_memory_callback(Fun) ->
    gen_server:call(?MODULE, {set_memory_callback, Fun}, infinity).

-spec get_memory() -> memory().
get_memory() ->
    gen_server:call(?MODULE, get_memory, infinity).

-spec get_latency() -> latency().
get_latency() ->
    gen_server:call(?MODULE, get_latency, infinity).

-spec get_transmission() -> transmission().
get_transmission() ->
    gen_server:call(?MODULE, get_transmission, infinity).

-spec stop_scheduling() -> ok.
stop_scheduling() ->
    gen_server:cast(?MODULE, stop_scheduling).

-spec record_transmission(transmission_type(), {timestamp(), metric()}) -> ok.
record_transmission(TransmissionType, {Timestamp, Size}) ->
    gen_server:cast(?MODULE, {transmission, TransmissionType,
        {Timestamp, Size}}).

%% @doc Record latency of:
%%          - `local': creating a message locally
%%          - `remote': delivering a message remotely
-spec record_latency(latency_type(), {timestamp(), metric()}) -> ok.
record_latency(Type, {Timestamp, MilliSeconds}) ->
    gen_server:cast(?MODULE, {latency, Type, {Timestamp, MilliSeconds}}).

%% gen_server callbacks
init([]) ->
    ?LOG("lmetrics initialized!"),
    schedule_memory(),
    {ok, #state{memory_callback=fun() -> undefined end,
                memory=[],
                keep_scheduling=true,
                latency=dict:new(),
                transmission=dict:new()}}.

handle_call({set_memory_callback, Fun}, _From, State) ->
    {reply, ok, State#state{memory_callback=Fun}};

handle_call(get_memory, _From,
            #state{memory=Memory}=State) ->
    ?LOG("lmetrics get_memory!"),
    {reply, lists:sort(Memory), State};

handle_call(get_latency, _From,
            #state{latency=Latency}=State) ->
    ?LOG("lmetrics get_latency!"),
    {reply, update_dict(Latency), State};

handle_call(get_transmission, _From,
            #state{transmission=Transmission}=State) ->
    ?LOG("lmetrics get_transmission!"),
    {reply, update_dict(Transmission), State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({transmission, TransmissionType, {Timestamp, Size}},
            #state{transmission=Transmission0}=State) ->
    Transmission1 = case dict:find(TransmissionType, Transmission0) of
        {ok, V} ->
            dict:store(TransmissionType,
                [{Timestamp, Size} | V], Transmission0);
        error ->
            dict:store(TransmissionType,
                [{Timestamp, Size}], Transmission0)
    end,
    {noreply, State#state{transmission=Transmission1}};

handle_cast({latency, Type, {Timestamp, MilliSeconds}},
            #state{latency=Latency0}=State) ->
    Latency1 = case dict:find(Type, Latency0) of
        {ok, V} ->
            dict:store(Type,
                [{Timestamp, MilliSeconds} | V], Latency0);
        error ->
            dict:store(Type,
                [{Timestamp, MilliSeconds}], Latency0)
    end,
    {noreply, State#state{latency=Latency1}};

handle_cast(stop_scheduling, State) ->
    {noreply, State#state{keep_scheduling=false}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(memory, #state{memory_callback=Fun,
    memory=Memory0, keep_scheduling=KeepScheduling}=State) ->
    Memory1 = case Fun() of
        undefined ->
            Memory0;
        {ok, ToBeAdded} ->
            Timestamp = lmetrics_util:get_timestamp(?UNIT),
            [{Timestamp, ToBeAdded}|Memory0]
    end,
    case KeepScheduling of
        true ->
            schedule_memory();
        false ->
            ok
    end,
    {noreply, State#state{memory=Memory1}};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
schedule_memory() ->
    Interval = lmetrics_config:get(memory_interval),
    timer:send_after(Interval, memory).

%% @private
update_dict(Dict) ->
    dict:fold(
        fun
            (K, V, Acc) ->
                dict:store(K, lists:sort(V), Acc)
        end,
        dict:new(),
        Dict).
