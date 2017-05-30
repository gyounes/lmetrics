-module(lmetrics_util).

-include("lmetrics.hrl").

%% ldb_util callbacks
-export([atom_to_binary/1,
         binary_to_atom/1,
         unix_timestamp/0,
         size/2]).

%% @doc
-spec atom_to_binary(atom()) -> binary().
atom_to_binary(Atom) ->
    erlang:atom_to_binary(Atom, utf8).

%% @doc
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
    erlang:binary_to_atom(Binary, utf8).

%% @doc
-spec unix_timestamp() -> timestamp().
unix_timestamp() ->
    {Mega, Sec, _Micro} = erlang:timestamp(),
    Mega * 1000000 + Sec.