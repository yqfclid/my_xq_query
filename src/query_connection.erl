%%%-------------------------------------------------------------------
%%% @author jack <jacktang@jackdeMacBook-Pro.local>
%%% @copyright (C) 2017, jack
%%% @doc
%%%
%%% @end
%%% Created : 24 Mar 2017 by jack <jacktang@jackdeMacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(query_connection).

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([start/0, query/2]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("my_xq_query.hrl").

-define(SERVER, ?MODULE).

-record(state, {cookie, pool }).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    case supervisor:start_child(query_connection_sup, []) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

query(Location, Code) ->
    case start() of
        {ok, Pid} ->
            gen_server:cast(Pid, {querying, Location, Code});
        {error, Reason} ->
            lager:error("~p start child failed: ~p", [?MODULE, Reason])
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Cookie, Pool) ->
    gen_server:start_link(?MODULE, [Cookie, Pool], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([Cookie, Pool]) ->
    {ok, #state{cookie = Cookie, pool = Pool}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    lager:warning("Can't handle request: ~p", [_Request]),
    {reply, {error, invalid_req}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({querying, Location, Code}, #state{pool = Pool, cookie = Cookie} = State) ->
    Url = <<?PATH/binary, Location/binary, Code/binary>>,
    Headers = [{cookie, Cookie}],
    Payload = <<>>,
    Opt = [{pool, Pool}],
    case hackney:request(get, Url, Headers, Payload, Opt) of
    	{ok, 200, _, Ref} ->
    		case hackney:body(Ref) of
    			{ok, Body} ->
                    NBody = jsx:decode(Body),
                    [{_, Infos}] = NBody,
                    Packet = encode_packet(Infos),
    				xq_udp:send(Packet);
    			Err ->
    				lager:error("read body failed: ~p", [Err])
            end;
        {ok, StateCode, _, _} ->
            lager:error("request failed:~p", [StateCode]);
        Err ->
            lager:error("request failed: ~p", [Err])
    end,
    {stop, normal, State};

handle_cast(_Msg, State) ->
    lager:warning("Can't handle msg: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason,  State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
encode_packet(Infos) ->
    {Tags, Fields, Time} = decode(Infos, <<>>, <<>>, undefined),
    <<?MEASUREMENT/binary, ",", Tags/binary, " ", Fields/binary, " ", Time/binary>>.

decode([], Tags, Fields, Time) ->
    {Tags, Fields, Time};
decode([{<<"time">>, Time}|T], Tags, Fields, _) ->
    NTime = decode_time(Time),
    decode(T, Tags, Fields, NTime);
decode([{K, _}|T], Tags, Fields, Time) when K =:= <<"market_status">>
                                       orelse K =:= <<"name">>->
    decode(T, Tags, Fields, Time);
decode([{K, <<>>}|T], Tags, Fields, Time) ->
    decode(T, Tags, Fields, Time);
decode([{K, V}|T], Tags, Fields, Time) when K =:= <<"symbol">>
                                       orelse K =:= <<"exchange">> 
                                       orelse K =:= <<"currency_unit">>->
    NTags =
        case Tags of
            <<>> ->
                <<K/binary, "=", V/binary>>; 
            _ ->
                <<Tags/binary, ",", K/binary, "=", V/binary>>
        end,
    decode(T, NTags, Fields, Time);
decode([{K, V}|T], Tags, Fields, Time) ->
    NV = binary:replace(V, <<"%">>, <<"">>),
    NFields =
        case Fields of
            <<>> ->
                <<K/binary, "=", NV/binary>>; 
            _ ->
                <<Fields/binary, ",", K/binary, "=", NV/binary>>
        end,
    decode(T, Tags, NFields, Time).


decode_time(Time) ->
    case binary:split(Time, <<" ">>) of
        [_, MonthB, DayB, DateTimeB, _, YearB] ->
            Month = convert_month(MonthB),
            Day = binary_to_integer(DayB),
            Year = binary_to_integer(YearB),
            <<HourB:16/bits, ":", 
              MinB:16/bits, ":",
              SecB:16/bits>> = DateTimeB,
            Hour = binary_to_integer(HourB),
            Min = binary_to_integer(MinB),
            Sec = binary_to_integer(SecB),
            Secs = cal_unix_time({{Year, Month, Day}, {Hour, Min, Sec}}),
            integer_to_binary(Secs * 1000000000);
        _ ->
            Secs = cal_unix_time(now),
            integer_to_binary(Secs * 1000000000)
    end.


cal_unix_time(Datetime) ->
    UnixTime =
        case Datetime of
            now ->
                calendar:universal_time();
            _ -> 
                [NDateTime] = calendar:local_time_to_universal_time_dst(Datetime),
                NDateTime
        end,
    calendar:datetime_to_gregorian_seconds(UnixTime) - 
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).


convert_month(<<"Jan">>) ->
    1;
convert_month(<<"Feb">>) ->
    2;
convert_month(<<"Mar">>) ->
    3;
convert_month(<<"Apr">>) ->
    4;
convert_month(<<"May">>) ->
    5;
convert_month(<<"June">>) ->
    6;
convert_month(<<"July">>) ->
    7;
convert_month(<<"Aug">>) ->
    8;
convert_month(<<"Sept">>) ->
    9;
convert_month(<<"Oct">>) ->
    10;
convert_month(<<"Nov">>) ->
    11;
convert_month(<<"Dec">>) ->
    12.