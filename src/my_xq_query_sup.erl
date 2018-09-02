%%%-------------------------------------------------------------------
%% @doc my_xq_query top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(my_xq_query_sup).

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
    InputFiles = application:get_env(my_xq_query, input_files, [{<<"SH">>, "priv/input_sh.data"},
                                                                {<<"SZ">>, "priv/input_sz.data"}]),
    SupFlags = 
    	#{strategy => one_for_one,
          intensity => 1000,
          period => 3600},
    Proc1 = 
    	#{id => query_connection_sup,              
      	  start => {query_connection_sup, start_link, []},
      	  restart => transient,
      	  shutdown => infinity,
      	  type => supervisor,
      	  modules => [query_connection_sup]},
    Proc2 = 
        #{id => xq_cookie_fresh,              
          start => {xq_cookie_fresh, start_link, []},
          restart => transient,
          shutdown => infinity,
          type => worker,
          modules => [xq_cookie_fresh]},
    Proc3 = 
        #{id => xq_collector,              
          start => {xq_collector, start_link, []},
          restart => transient,
          shutdown => infinity,
          type => worker,
          modules => [xq_collector]},
    Proc4 = 
        #{id => xq_manager,              
          start => {xq_manager, start_link, [InputFiles]},
          restart => transient,
          shutdown => infinity,
          type => worker,
          modules => [xq_manager]},
    {ok, { SupFlags, [Proc1, 
                      Proc4,
                      Proc2,
                      Proc3]} }.

%%====================================================================
%% Internal functions
%%====================================================================
