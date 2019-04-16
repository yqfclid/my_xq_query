%%%-------------------------------------------------------------------
%%% @author Yqfclid 
%%% @copyright  Yqfclid (yqf@blackbird)
%%% @doc
%%%
%%% @end
%%% Created :  2019-04-11 15:38:34
%%%-------------------------------------------------------------------
-module(xq_tick_signal).

-behaviour(gen_server).

%% API
-export([get_status/0]).
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

-record(state, {trading_duration, timer, signal_id, status}).

%%%===================================================================
%%% API
%%%===================================================================
get_status() ->
    gen_server:call(?MODULE, status).
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
    {Id, Status, NextSt, Interval} = check_status(),
    NId = next_id(Id),
    Timer = erlang:start_timer(Interval, self(), {change_status, {NId, NextSt}}),
    {ok, #state{trading_duration = ?TRADING_DURATION,
                timer = Timer,
                status = Status,
                signal_id = Id}}.

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
handle_call(status, _From, #state{status = Status} = State) ->
    {reply, Status, State};

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
handle_info({timeout, Timer, {change_status, {Id, Status}}}, #state{timer = Timer,
                                                                    trading_duration = Durations} = State) ->
    lists:foreach(fun({Pid, _}) -> Pid ! {change_status, Status} end, ets:tab2list(?SYMBOLS_TAB)),
    {NId, NextSt, Interval} = next_status(Id, Durations),
    NTimer = erlang:start_timer(Interval, self(), {change_status, {NId, NextSt}}),
    {noreply, State#state{timer = NTimer,
                          status = Status,
                          signal_id = Id}};

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
check_status() ->
    Now = erlang:time(),
    Date = erlang:date(),
    check_status(Date, Now, ?TRADING_DURATION).

check_status(Date, Time, Duration) ->
    case calendar:day_of_the_week(Date) of
        WeekDay when WeekDay >5 ->
            [{{H, M, S}, _}|_] = Duration,
            Interval = ((7 - WeekDay) * 86400 + H * 3600 + M * 60 + S) * 1000, 
            {5, off, on, Interval};
        _ ->
            check_status_1(Duration, Time)
    end. 

check_status_1([{_, _}, {_, {H4, M4, S4} = D4}], {H, M, S} = Time) when Time >= D4 ->
    {4, off, on, (H * 3600 + M * 60 + S + 86400 - H4 * 3600 - M4 * 60 - S4) * 1000};
check_status_1([{{H1, M1, S1} = D1, _}, {_, _}], {H, M, S} = Time) when Time < D1 ->
    {4, off, on, ((H1 - H) * 3600 + (M1 - M) * 60 + (S1 - S)) * 1000};
check_status_1([{D1, {H2, M2, S2} = D2}, {_, _}], {H, M, S} = Time) when Time >= D1 andalso Time =< D2 ->
    {1, on, off, ((H2 - H) * 3600 + (M2 - M) * 60 + (S2 - S)) * 1000};
check_status_1([{_, D2}, {{H3, M3, S3} = D3, _}], {H, M, S} = Time) when Time > D2 andalso Time < D3 ->
    {2, off, on, ((H3 - H) * 3600 + (M3 - M) * 60 + (S3 - S)) * 1000};
check_status_1([{_, _}, {D3, {H4, M4, S4} = D4}], {H, M, S} = Time) when Time >= D3 andalso Time < D4 ->
    {3, on, off, ((H4 - H) * 3600 + (M4 - M) * 60 + (S4 - S)) * 1000}.

next_status(1, [{{H1, M1, S1}, {H2, M2, S2}}, {_, _}]) -> 
    {2, off, ((H2 - H1) * 3600 + (M2 - M1) * 60 + (S2 - S1)) * 1000};
next_status(2, [{_, {H2, M2, S2}}, {{H3, M3, S3}, _}]) ->
    {3, on, ((H3 - H2) * 3600 + (M3 - M2) * 60 + (S3 - S2)) * 1000};
next_status(3, [{_, _}, {{H3, M3, S3}, {H4, M4, S4}}]) ->
    {4, off, ((H4 - H3) * 3600 + (M4 - M3) * 60 + (S4 - S3)) * 1000};
next_status(4, [{{H1, M1, S1}, _}, {_, {H4, M4, S4}}]) ->
    case calendar:day_of_the_week(erlang:date()) of
        5 ->
            {1, on, (3 * 86400 + H1 * 3600 + M1 * 60 + S1 - H4 * 3600 - M4 * 60 - S4) * 1000};
        _ ->
            {1, on, (86400 + H1 * 3600 + M1 * 60 + S1 - H4 * 3600 - M4 * 60 - S4) * 1000}
    end.

next_id(1) -> 2;
next_id(2) -> 3;
next_id(3) -> 4;
next_id(4) -> 1.