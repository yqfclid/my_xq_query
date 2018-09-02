%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2018-08-29 23:38:06
%%%-------------------------------------------------------------------
-module(xq_collector).

-behaviour(gen_server).

%% API
-export([collect_market/1,
		 subscribe/1,
		 unsubscribe/1]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("my_xq_query.hrl").

-record(state, {subscribers}).

%%%===================================================================
%%% API
%%%===================================================================
collect_market({Symbol, Market}) ->
	gen_server:cast(?SERVER, {market, Symbol, Market}).

subscribe(Pid) ->
	gen_server:call(?SERVER, {subscribe, Pid}).

unsubscribe(Pid) ->
	gen_server:call(?SERVER, {unsubscribe, Pid}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
init([]) ->
	process_flag(trap_exit, true),
    {ok, #state{subscribers = sets:new()}}.

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
handle_call({subscribe, Pid}, _From, State) ->
	#state{subscribers = Subs} = State,
	link(Pid),
	NSubs = sets:add_element(Pid, Subs),
	{reply, ok, State#state{subscribers = NSubs}};

handle_call({unsubscribe, Pid}, _From, State) ->
	#state{subscribers = Subs} = State,
	unlink(Pid),
	NSubs = sets:del_element(Pid, Subs),
	{reply, ok, State#state{subscribers = NSubs}};

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
handle_cast({market, Symbol, Market}, State) ->
	#state{subscribers = Subs} = State,
	sets:fold( 
		fun(Pid, _) ->
			Pid ! Market
	end, ok, Subs),
	{noreply, State};

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
handle_info({'EXIT', Pid, Why}, #state{subscribers=Subs}=State) ->
    lager:error("Subscriber ~p disconnected ~p~n", [Pid, Why]),
    NSubs = sets:del_element(Pid, Subs),
    {noreply, State#state{subscribers = NSubs}};

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
