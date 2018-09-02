%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-29 23:42:48
%%%-------------------------------------------------------------------
-module(xq_utils).

-export([http_request/5,
		 get_cookie/1,
		 generate_name/2,
		 spilt_code/2]).

-include("my_xq_query.hrl").

%%%===================================================================
%%% API
%%%===================================================================
http_request(Method, Url, Headers, Body, Options) ->
	case hackney:request(Method, Url, Headers, Body, Options) of
		{ok, StatusCode, _RetHeaders, Ref} ->
			case erlang:trunc(StatusCode/100) of
				2 ->
					hackney:body(Ref);
				_Other ->
					lager:error("~p from ~p with headers ~p body ~p failed:~p", 
						[Method, Url, Headers, Body, _Other]),
					hackney:close(Ref),
					{error, StatusCode}
			end;
		{error, Reason} ->
			lager:error("~p from ~p with headers ~p body ~p failed:~p", 
				[Method, Url, Headers, Body, Reason]),	
			{error, Reason}
	end.

get_cookie(Headers) ->
	lists:foldl(
		fun({<<"Set-Cookie">>, SetedCookie}, Acc) ->
			[{CookieK, CookieV}|_] = hackney_cookie:parse_cookie(SetedCookie),
			<<Acc/binary, CookieK/binary, "=", CookieV/binary, ";">>;
		   (_, Acc) ->
		   	Acc
	end, <<>>, Headers).

generate_name(NameSpace, Id) when is_atom(NameSpace)->
    generate_name(atom_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_binary(NameSpace) ->
    generate_name(binary_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_integer(NameSpace) ->
    generate_name(integer_to_list(NameSpace), Id);
generate_name(NameSpace, Id) when is_list(NameSpace)->
    list_to_atom(NameSpace ++ "_" ++ integer_to_list(erlang:phash2(Id)));
generate_name(_, _) ->
    throw(badarg).

spilt_code(Binary, TermOutFun)->
	spilte(TermOutFun, Binary, <<>>).
%%%===================================================================
%%% Internal functions
%%%===================================================================
spilte(TermOutFun, <<>>, _)->
	TermOutFun(over);
spilte(TermOutFun,<<"(", B1, B2, B3, B4, B5, B6, ")", Binary/binary>>, LastName) ->
	Code = <<B1, B2, B3, B4, B5, B6>>,
	TermOutFun({LastName, Code}),
	spilte(TermOutFun, Binary, <<>>);
spilte(TermOutFun, <<"\n",Binary/binary>>, LastName) ->
	spilte(TermOutFun, Binary, LastName);
spilte(TermOutFun,<<C, Binary/binary>>,LastName) ->
	LastName2 = <<LastName/binary, C>>,
	spilte(TermOutFun,Binary,LastName2).