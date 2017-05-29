-define(APP, lmetrics).
-type not_found() :: {error, not_found}.
-type error() :: {error, atom()}.

%% lmetrics
-type key() :: string().
-type type() :: term().
-type value() :: term().
-type message() :: term().
-type timestamp() :: non_neg_integer().
