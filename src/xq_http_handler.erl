%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-30 12:30:47
%%%-------------------------------------------------------------------
-module(xq_http_handler).

-export([init/2]).

-define(POOL, xq_http_pool).

-include("my_xq_query.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Req0, Opts) ->
	lager:info("receive raw http reqest: ~p", [Req0]),
	Method = cowboy_req:method(Req0),
	#{code := Code} = cowboy_req:match_qs([{type, [], undefined}], Req0),
	Req1 = handle_req(Method, Code, Req0),
	{ok, Req1, Opts}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_req(<<"GET">>, Code, Req0) ->
	Url = <<?MARKET_URL/binary, Code/binary>>,
	Reply = 
		case ets:lookup(?XQ_TAB, cookie) of
			[{_, Cookie}] ->
				case xq_utils:http_request(get, Url, [{cookie, Cookie}], <<>>, [{ssl_options, [{depth, 2}]}, 
																				{pool, ?POOL}]) of
					{ok, Body} ->
						Ret = jiffy:decode(Body, [return_maps]),
						[CodeRet|_] = maps:keys(Ret),
						#{CodeRet := Info} = Ret,
						{ok, Info};
					{error, Reason} ->
						{error, Reason}
				end;
			_Other ->
				lager:error("find cookie failed: ~p", [_Other])
		end,
	cowboy_req:reply(200, [], build_reply(Reply), Req0);
handle_req(_Method, _, Req) ->
	cowboy_req:reply(400, [], <<"invalid request">>, Req).	

build_reply({error, Reason}) ->
	Reply = 
		case Reason of
			_ ->
				lager:error("system get unexcept error:~p", [Reason]),
				<<"未知错误"/utf8>>
		end,
	jiffy:encode(#{<<"return_code">> => <<"1">>,
				   <<"reason">> => Reply});
build_reply({ok, ok}) ->
	jiffy:encode(#{<<"return_code">> => <<"0">>});
build_reply({ok, RtnMap}) ->
	NMap = maps:put(<<"return_code">>, <<"0">>, RtnMap),
	jiffy:encode(NMap).