%%%-------------------------------------------------------------------
%% @doc my_xq_query top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(query_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Interval = application:get_env(my_xq_query, query_interval, 5000),
    SupFlags = 
      #{strategy => simple_one_for_one,
          intensity => 1000,
          period => 3600},
    Proc1 = 
      #{id => query_connection,              
          start => {query_connection, start_link, [Interval]},
          restart => transient,
          shutdown => infinity,
          type => supervisor,
          modules => [query_connection]},
    {ok, { SupFlags, [Proc1]} }.

%%====================================================================
%% Internal functions
%%====================================================================
