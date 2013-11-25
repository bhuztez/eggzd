-module(eggzd_app).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).


start(normal, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init(_) ->
    {ok,
     {
       {one_for_one, 0, 1},
       [
	{ eggzd_c2s_listener,
	  {eggzd_c2s_listener, start_link, []},
	  permanent,
	  brutal_kill,
	  worker,
	  [eggzd_c2s_listener]
	},
	{ eggzd_c2s_sup,
	  {eggzd_c2s_sup, start_link, []},
	  permanent,
	  brutal_kill,
	  supervisor,
	  [eggzd_c2s_sup]
	}
       ]
     }
    }.
