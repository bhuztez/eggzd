-module(eggzd_c2s_listener).

-behaviour(supervisor_bridge).

-export([start_link/0]).

-export([init/1, terminate/2]).

-export([loop/1]).


start_link() ->
    supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    Port = application:get_env(eggzd, port, 5688),
    {ok, Socket} =
	gen_tcp:listen(
	  Port,
	  [binary,
	   {packet, raw},
	   {reuseaddr, true},
	   {active, false}]),
    {ok, spawn_link(?MODULE, loop, [Socket]), Socket}.

loop(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    supervisor:start_child(eggzd_c2s_sup, [Conn]),
    loop(Socket).

terminate(_Reason, Socket) ->
    gen_tcp:close(Socket),
    ok.
