-module(my_xq_query_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, my_xq_query).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	InfluxConf = application:get_env(?APP, influx_udp, []),
	Host = proplists:get_value(host, InfluxConf, "localhost"),
	Port = proplists:get_value(port, InfluxConf, 38086),
	InputFiles = application:get_env(?APP, input_files, []),
	Cookie = application:get_env(?APP, cookie, []),
    Interval = application:get_env(?APP, interval, 3000),
    Pool = application:get_env(?APP, pool, default),
    {ok, { {one_for_one, 5, 10}, [
        {query_connection_sup, {supervisor, start_link, [{local, query_connection_sup}, my_xq_query_sup, [query_connection_sup, Cookie, Pool]]},
            transient, 5000, supervisor, [query_connection_sup]},
        {query_process_sup, {supervisor, start_link, [{local, query_process_sup}, my_xq_query_sup, [query_process_sup, Interval]]},
            transient, 5000, supervisor, [query_process_sup]},
        {xq_udp, {xq_udp, start_link, [Host, Port]},
            transient, 5000, worker, [xq_udp]},
        {query_manager, {query_manager, start_link, [InputFiles]},
            transient, 5000, worker, [query_manager]}]} };


init([query_connection_sup, Cookie, Pool]) ->
	{ok, {{simple_one_for_one, 5, 10},
		  [{query_connection, {query_connection, start_link, [Cookie, Pool]},
            transient, 5000, worker, [query_connection]}]}};

init([query_process_sup, Interval]) ->
	{ok, {{simple_one_for_one, 5, 10},
		  [{query_process, {query_process, start_link, [Interval]},
            transient, 5000, worker, [query_process]}]}}.