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
         generate_name/2]).

-export([timestamp/0,
         date/0]).

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

timestamp() ->
    os:system_time().

date() ->
    {Y, M, D} = erlang:date(),
    list_to_binary(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Y, M, D])). 
%%%===================================================================
%%% Internal functions
%%%===================================================================