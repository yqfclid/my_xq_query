%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-29 23:42:18
%%%-------------------------------------------------------------------
-module(query_connection).

-behaviour(gen_server).

%% API
-export([start/2,
		 stop/1,
		 change_interval/2]).

-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("my_xq_query.hrl").

-record(state, {symbol, timer, interval}).

%%%===================================================================
%%% API
%%%===================================================================
change_interval(Symbol, NInterval) when is_integer(NInterval) 
								   andalso is_binary(Symbol) ->
	PName = xq_utils:generate_name(?MODULE, Symbol),
	gen_server:cast(PName, {change_interval, NInterval});
change_interval(_, _) ->
	{error, bad_arg}.

stop(Symbol) ->
	PName = xq_utils:generate_name(?MODULE, Symbol),
	gen_server:cast(PName, stop).

start(Area, Code) ->
	case supervisor:start_child(query_connection_sup, [Area, Code]) of
		{ok, Pid} ->
			{ok, Pid};
        {ok, Pid, _Info} -> 
        	{ok, Pid};
        {error, {already_started, Pid}} -> 
        	{ok, Pid};
        {error, Reason} -> 
        	{error, Reason}
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Interval, Area, Code) ->
	Symbol = <<Area/binary, Code/binary>>,
	PName = xq_utils:generate_name(?MODULE, Symbol),
    gen_server:start_link({local, PName}, ?MODULE, [Interval, Symbol], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the SERVER
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Interval, Symbol]) ->
	self() ! query_market,
    {ok, #state{symbol = Symbol,
    			timer = make_ref(),
    			interval = Interval}}.

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
    Reply = ok,
    lager:warning("Can't handle request: ~p", [_Request]),
    {reply, Reply, State}.

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
handle_cast({change_interval, Interval}, State) ->
	{noreply, State#state{interval = Interval}};

handle_cast(stop, #state{symbol = Symbol} = State) ->
	lager:info("stop query ~p market process", [Symbol]),
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
handle_info(query_market, #state{symbol = Symbol,
								 timer = Timer,
								 interval = Interval} = State) ->
	erlang:cancel_timer(Timer),
	Url = <<?MARKET_URL/binary, Symbol/binary>>,
	case ets:lookup(?XQ_TAB, cookie) of
		[{_, Cookie}] ->		
			case xq_utils:http_request(get, 
									   Url, 
									   [{cookie, Cookie}], 
									   <<>>, 
									   [{ssl_options, [{depth, 2}]}]) of
				{ok, Body} ->
					Ret = jiffy:decode(Body, [return_maps]),
					[Key|_] = maps:keys(Ret),
					#{Key := Market} = Ret,
					xq_collector:collect_market({Symbol, Market});
				{error, Reason} ->
					lager:error("query ~p market failed:~p", [Symbol, Reason])
			end;
		[] ->
			lager:error("no cookie find");
		{error, Reason} ->
			lager:error("find cookie failed:~p", [Reason])
	end,
	NTimer = erlang:send_after(Interval, self(), query_market),
	{noreply, State#state{timer = NTimer}};

handle_info(_Info, State) ->
    lager:warning("Can't handle info: ~p", [_Info]),
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
terminate(_Reason, _State) ->
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
