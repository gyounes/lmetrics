-define(APP, lmetrics).
-type not_found() :: {error, not_found}.
-type error() :: {error, atom()}.

%% lmetrics
-type timestamp() :: non_neg_integer().

-define(UNIT, millisecond).
-define(DEFAULT_LMETRICS_TS_INTERVAL, 1000).

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