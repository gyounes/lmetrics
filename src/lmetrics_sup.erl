%%%-------------------------------------------------------------------
%% @doc lmetrics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lmetrics_sup).

-behaviour(supervisor).

-include("lmetrics.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    configure(),
    Backend = [{lmetrics,
               {lmetrics, start_link, []},
               permanent, 5000, worker, [lmetrics]}],

    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Backend}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
configure() ->
    %% configure Interval
    configure_int("LMETRICS_TS_INTERVAL",
                  time_series_interval,
                  ?DEFAULT_LMETRICS_TS_INTERVAL).

%% @private
configure_int(Env, Var, Default) ->
    To = fun(V) -> integer_to_list(V) end,
    From = fun(V) -> list_to_integer(V) end,
    configure(Env, Var, Default, To, From).

%% @private
configure(Env, Var, Default, To, From) ->
    Current = lmetrics_config:get(Var, Default),
    Val = From(
        os:getenv(Env, To(Current))
    ),
    lmetrics_config:set(Var, Val),
    Val.