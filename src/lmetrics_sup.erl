%%%-------------------------------------------------------------------
%% @doc lmetrics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lmetrics_sup).

-behaviour(supervisor).

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

	Backend = [{lmetrics,
               {lmetrics, start_link, []},
               permanent, 5000, worker, [lmetrics]}],

    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Backend}}.

%%====================================================================
%% Internal functions
%%====================================================================
