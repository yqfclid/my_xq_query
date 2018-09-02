%%%-------------------------------------------------------------------
%% @doc my_xq_query public API
%% @end
%%%-------------------------------------------------------------------

-module(my_xq_query_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	lager:start(),
    case my_xq_query_sup:start_link() of
    	{ok, Pid} ->
    		HttpPort = application:get_env(my_xq_query, http_port, 38086),
			Dispatch = cowboy_router:compile([
				{'_', [
					{"/market", xq_http_handler, []}
				]}
			]),
			{ok, _} = cowboy:start_clear(http, [{port, HttpPort}], #{
				env => #{dispatch => Dispatch}
			}),
    		{ok, Pid};
    	Error ->
    		Error
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
