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
-export([start/1,
         stop/1,
         change_interval/2,
         force_change_status/2]).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("my_xq_query.hrl").

-record(state, {symbol, timer, interval, status}).

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

start(Symbol) ->
    case supervisor:start_child(query_connection_sup, [Symbol]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} -> 
            {ok, Pid};
        {error, {already_started, Pid}} -> 
            {ok, Pid};
        {error, Reason} -> 
            {error, Reason}
    end.

force_change_status(Symbol, Status) ->
    PName = xq_utils:generate_name(?MODULE, Symbol),
    PName ! {change_status, Status}.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Interval, Symbol) ->
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
    Status = xq_tick_signal:get_status(),
    TimeRef = erlang:start_timer(Interval, self(), query_market),
    {ok, #state{symbol = Symbol,
                interval = Interval,
                status = Status,
                timer = TimeRef}}.

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
handle_info({timeout, Timer, query_market}, #state{symbol = Symbol,
                                                   timer = Timer,
                                                   interval = Interval,
                                                   status = on} = State) ->
    Url = <<?MARKET_URL/binary, Symbol/binary>>,
    case ets:lookup(?COOKIE_TAB, cookie) of
        [{_, Cookie}] ->        
            case xq_utils:http_request(get, 
                                       Url, 
                                       [{cookie, Cookie}], 
                                       <<>>, 
                                       [{ssl_options, [{depth, 2}]}]) of
                {ok, Body} ->
                    Ret = jiffy:decode(Body, [return_maps]),
                    [Key|_] = maps:keys(Ret),
                    #{Key := Detail} = Ret,
                    Time = xq_utils:timestamp(),
                    Date = xq_utils:date(),
                    Market = #market{symbol = Symbol,
                                     time = Time,
                                     date = Date, 
                                     detail = Detail},
                    xq_collector:collect_market(Market);
                {error, Reason} ->
                    lager:error("query ~p market failed:~p", [Symbol, Reason])
            end;
        [] ->
            lager:error("no cookie find");
        {error, Reason} ->
            lager:error("find cookie failed:~p", [Reason])
    end,
    NTimer = erlang:start_timer(Interval, self(), query_market),
    {noreply, State#state{timer = NTimer}};

handle_info({timeout, Timer, query_market}, #state{timer = Timer,
                                                   interval = Interval} = State) ->
    NTimer = erlang:start_timer(Interval, self(), query_market),
    {noreply, State#state{timer = NTimer}};

handle_info({change_status, Status}, State) ->
    {noreply, State#state{status = Status}};

handle_info({status, Status}, State) ->
    {noreply, State#state{status = Status}};

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
