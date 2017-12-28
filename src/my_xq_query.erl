-module(my_xq_query).

%% Application callbacks
-export([start/0, stop/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start()->
    application:ensure_all_started(my_xq_query).

stop() ->
    application:stop(my_xq_query).