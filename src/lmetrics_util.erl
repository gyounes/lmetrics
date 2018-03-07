-module(lmetrics_util).

-include("lmetrics.hrl").

%% ldb_util callbacks
-export([atom_to_binary/1,
         binary_to_atom/1,
         get_timestamp/1]).

%% @doc
-spec atom_to_binary(atom()) -> binary().
atom_to_binary(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

%% @doc
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
    erlang:binary_to_atom(Binary, utf8).

%% @private get current time
-spec get_timestamp(term()) -> term().
get_timestamp(Unit) ->
    erlang:monotonic_time(Unit).