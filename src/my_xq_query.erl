%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-29 23:27:00
%%%-------------------------------------------------------------------
-module(my_xq_query).

-export([start/0,
		 stop/0,
		 subscribe/1,
		 unsubscribe/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	application:ensure_all_started(my_xq_query).

stop() ->
	application:stop(my_xq_query).

subscribe(Pid) ->
	xq_collector:subscribe(Pid).

unsubscribe(Pid) ->
	xq_collector:unsubscribe(Pid).

%%%===================================================================
%%% Internal functions
%%%===================================================================
