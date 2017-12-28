%%%-------------------------------------------------------------------
%%% @author Yqfclid
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(query_process).

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

-define(SERVER, ?MODULE).

-record(state, {code, interval, location}).

%%%===================================================================
%%% API
%%%===================================================================
start(Code, Location) ->
    case supervisor:start_child(query_process_sup, [Code, Location]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.
stop(Code) ->
    PName = "process_" ++ binary_to_list(Code),
    gen_server:cast(PName, stop).
       
change_interval(Code, Interval) ->
    PName = "process_" ++ binary_to_list(Code),
    PName ! {interval, Interval}.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Interval, Code, Location) ->
    PName = list_to_atom("process_" ++ binary_to_list(Code)), 
    gen_server:start_link({local, PName}, ?MODULE, [Interval, Code, Location], []).

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

init([Interval, Code, Location]) ->
    hackney_pool:start_pool(Code, [{max_connection, 100}]),
    self() ! start_query,
    {ok, #state{code = Code, location = Location, interval = Interval}}.

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
handle_cast(stop, State) ->
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
handle_info(start_query, #state{} = State) ->
    #state{location = Location,
           interval  =Interval,
           code = Code} = State,
    query_connection:query(Location, Code),
    timer:send_after(Interval, self(), start_query),
    {noreply, State};

handle_info({interval, Interval}, State) ->
    {noreply, State#state{interval = Interval}};

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
terminate(_Reason, State) ->
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

