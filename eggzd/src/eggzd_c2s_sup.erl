-module(eggzd_c2s_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok,
     {
       {simple_one_for_one, 0, 1},
       [
	{ eggzd_c2s,
	  {eggzd_c2s, start_link, []},
	  temporary,
	  1,
	  worker,
	  [eggzd_c2s]
	}
       ]
     }
    }.
