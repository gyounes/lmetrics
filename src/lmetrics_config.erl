-module(lmetrics_config).

-include("lmetrics.hrl").

-export([get/1,
         get/2,
         set/2]).

%% @doc
-spec get(atom()) -> term().
get(Property) ->
    {ok, Value} = application:get_env(?APP, Property),
    Value.

-spec get(atom(), term()) -> term().
get(Property, Default) ->
    application:get_env(?APP, Property, Default).

-spec set(atom(), term()) -> ok.
set(Property, Value) ->
    application:set_env(?APP, Property, Value).