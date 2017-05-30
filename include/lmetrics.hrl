-define(APP, lmetrics).
-type not_found() :: {error, not_found}.
-type error() :: {error, atom()}.

%% lmetrics
-type key() :: string().
-type type() :: term().
-type value() :: term().
-type message() :: term().
-type timestamp() :: non_neg_integer().

%% logging
-define(LOGGING, list_to_atom("true")). %% dialyzer
-define(LOG(S),
        ?LOG(S, [])
       ).
-define(LOG(S, Args),
        case ?LOGGING of
            true ->
                lager:info(S, Args);
            false ->
                ok
        end
       ).