-module(lmetrics).

-include("lmetrics.hrl").

-behaviour(gen_server).

%% lmetrics callbacks
-export([start_link/0,
         set_time_series_callback/1,
         get_time_series/0,
         get_latency/0,
         record_message/2,
         record_latency/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type metric_type() :: transmission | memory.
-type metric() :: term().
-type time_series() :: list({timestamp(), metric_type(), metric()}).

-type latency_type() :: local | remote.
-type latency() :: list({latency_type(), list(integer())}).

-record(state, {time_series_callback :: function(),
                time_series :: time_series(),
                latency_type_to_latency :: orddict:orddict()}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_time_series_callback(function()) -> ok.
set_time_series_callback(Fun) ->
    gen_server:call(?MODULE, {set_time_series_callback, Fun}, infinity).

-spec get_time_series() -> time_series().
get_time_series() ->
    gen_server:call(?MODULE, get_time_series, infinity).

-spec get_latency() -> latency().
get_latency() ->
    gen_server:call(?MODULE, get_latency, infinity).

-spec record_message(term(), non_neg_integer()) -> ok.
record_message(MessageType, Size) ->
    Timestamp = lmetrics_util:unix_timestamp(),
    gen_server:cast(?MODULE, {message, Timestamp, MessageType, Size}).

%% @doc Record latency of:
%%          - `local': creating a message locally
%%          - `remote': applying a message remotely
-spec record_latency(latency_type(), integer()) -> ok.
record_latency(Type, MicroSeconds) ->
    gen_server:cast(?MODULE, {latency, Type, MicroSeconds}).

%% gen_server callbacks
init([]) ->
    schedule_time_series(),
    ?LOG("lmetrics initialized!"),
    {ok, #state{time_series_callback=fun() -> undefined end,
                time_series=[],
                latency_type_to_latency=orddict:new()}}.

handle_call({set_time_series_callback, Fun}, _From, State) ->
    {reply, ok, State#state{time_series_callback=Fun}};

handle_call(get_time_series, _From,
            #state{time_series=TimeSeries}=State) ->
    {reply, TimeSeries, State};

handle_call(get_latency, _From,
            #state{latency_type_to_latency=Map}=State) ->
    {reply, Map, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({message, Timestamp, MessageType, Size},
            #state{time_series=TimeSeries0}=State) ->

    Timestamp = lmetrics_util:unix_timestamp(),
    TMetric = {Timestamp, transmission, {MessageType, Size}},
    TimeSeries1 = lists:append(TimeSeries0, [TMetric]),

    {noreply, State#state{time_series=TimeSeries1}};

handle_cast({latency, Type, MicroSeconds},
            #state{latency_type_to_latency=Map0}=State) ->
    Map1 = orddict:append(Type, MicroSeconds, Map0),
    {noreply, State#state{latency_type_to_latency=Map1}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(time_series, #state{time_series_callback=Fun,
                                time_series=TimeSeries0}=State) ->
    TimeSeries1 = case Fun() of
        undefined ->
            TimeSeries0;
        {ok, ToBeAdded} ->
            Timestamp = lmetrics_util:unix_timestamp(),
            TMetric = {Timestamp, memory, ToBeAdded},
            lists:append(TimeSeries0, [TMetric])
    end,
    schedule_time_series(),
    {noreply, State#state{time_series=TimeSeries1}};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
schedule_time_series() ->
    %% @todo
    %% Interval = lmetrics_config:get(time_series_interval),
    Interval = 2000,
    timer:send_after(Interval, time_series).
