-module(eggzd_c2s).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_handler/2]).


-include_lib("xmerl/include/xmerl.hrl").
-include("eggzd.hrl").


-record(state,
	{
	  conn,      %% socket
	  handler,   %% handler Pid
	  language,  %% client language
	  user = not_logged_in
	}).


start_link(Conn) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Conn], []).


init([Conn]) ->
    process_flag(trap_exit, true),
    Handler = spawn_link(?MODULE, start_handler, [Conn, self()]),

    {ok, Vsn} = application:get_key(eggzd, vsn),
    {ok, Name} = application:get_env(eggzd, name),

    gen_tcp:send(
      Conn,
      <<"<SESSION>">>),
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'SERVER',
	   [{'ID', "EGGZD-"++Vsn},
	    {'NAME', Name},
	    {'VERSION', ?GGZ_C2S_PROTO_VERSION},
	    {'STATUS', "ok"},
	    {'TLS_SUPPORT', "no"}],
	   []}),
	xmerl_xml)),
    {ok, #state{conn=Conn, handler=Handler}}.


handle_call({language, Language}, {Handler, _}, #state{handler=Handler}=State) ->
    {reply, ok, State#state{language=Language}};
handle_call({login, [{type, Type}|Params]}, {Handler, _}, #state{handler=Handler, conn=Conn, user=not_logged_in}=State) ->
    NextState =
	case Type of
	    normal ->
		{name, Name} = proplists:lookup(name, Params),
		State#state{user={user, Name}};
	    guest ->
		State#state{user=guest};
	    first ->
		{name, Name} = proplists:lookup(name, Params),
		State#state{user={user, Name}}
	end,
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "login"},
	    {'CODE', "ok"}],
	   []}),
	xmerl_xml)),
    {reply, ok, NextState};
handle_call({login, _}, {Handler, _}, #state{handler=Handler, conn=Conn}=State) ->
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "login"},
	    {'CODE', "already logged in"}],
	   []}),
	xmerl_xml)),
    {reply, ok, State};
handle_call({list, [{type, room}, {full, _Full}]}, {Handler, _}, #state{handler=Handler, conn=Conn}=State) ->
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "list"},
	    {'CODE', "ok"}],
	   [{'LIST',
	     [{'TYPE', "room"}],
	     []}]}),
	xmerl_xml)),
    {reply, ok, State};
handle_call({list, [{type, game}]}, {Handler, _}, #state{handler=Handler, conn=Conn}=State) ->
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "list"},
	    {'CODE', "ok"}],
	   [{'LIST',
	     [{'TYPE', "game"}],
	     []}]}),
	xmerl_xml)),
    {reply, ok, State};
handle_call({list, [{type, player}]}, {Handler, _}, #state{handler=Handler, conn=Conn}=State) ->
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "list"},
	    {'CODE', "ok"}],
	   [{'LIST',
	     [{'TYPE', "player"}],
	     []}]}),
	xmerl_xml)),
    {reply, ok, State};
handle_call({list, [{type, table}]}, {Handler, _}, #state{handler=Handler, conn=Conn}=State) ->
    gen_tcp:send(
      Conn,
      xmerl:export_element(
	xmerl_lib:expand_element(
	  {'RESULT',
	   [{'ACTION', "list"},
	    {'CODE', "ok"}],
	   [{'LIST',
	     [{'TYPE', "table"}],
	     []}]}),
	xmerl_xml)),
    {reply, ok, State}.



handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({'EXIT', Handler, Reason}, #state{handler=Handler}=State) ->
    {stop, Reason, State}.


terminate(_Reason, #state{conn=Conn}) ->
    gen_tcp:close(Conn),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



start_handler(Conn, Controller) ->
    link(Controller),
    xmerl_sax_parser:stream(
      <<"">>,
      [skip_external_dtd,
       {continuation_fun, fun read_some/1},
       {continuation_state, Conn},
       {event_fun, fun event_cb/3},
       {event_state, {Controller, none}}]).


read_some(Conn) ->
    case gen_tcp:recv(Conn, 0) of
	{ok, Data} ->
	    {Data, Conn};
	{error, closed} ->
	    throw({lost, "Connection Lost"})
    end.


event_cb(startDocument, _Location, {Controller, none}) ->
    {Controller, start};

event_cb(endDocument, _Location, State) ->
    State;

event_cb({ignorableWhitespace, _}, _Location, State) ->
    State;

event_cb({startElement, "", Tag, {"", Tag}, Attribs}, _Location, {Controller, State}) ->
    NextState =
	case {Tag, Attribs, State} of
	    {"SESSION", [], start} ->
		[];

	    {"LANGUAGE", [], []} ->
		[{language, ""}];

	    {"LOGIN", [{"", "", "TYPE", "normal"}], []} ->
		[{login, [{type, normal}]}];
	    {"LOGIN", [{"", "", "TYPE", "guest"}], []} ->
		[{login, [{type, guest}]}];
	    {"LOGIN", [{"", "", "TYPE", "first"}], []} ->
		[{login, [{type, first}]}];
	    {"NAME", [], [{login, _}]} ->
		[{name, ""}|State];
	    {"PASSWORD", [], [{login, [{type, normal}|_]}]} ->
		[{password, ""}|State];
	    {"PASSWORD", [], [{login, [{type, first}|_]}]} ->
		[{password, ""}|State];
	    {"EMAIL", [], [{login, [{type, first}|_]}]} ->
		[{email, ""}|State];

	    {"LIST", _, []} ->
		Attrs = [{K,V} || {_,_,K,V} <- Attribs],

		case proplists:get_value("TYPE", Attrs) of
		    "room" ->
			case proplists:get_value("FULL", Attrs) of
			    "true" ->
				[{list, [{type, room}, {full, true}]}];
			    "false" ->
				[{list, [{type, room}, {full, false}]}]
			end;
		    "game" ->
			[{list, [{type, game}]}];
		    "player" ->
			[{list, [{type, player}]}];
		    "table" ->
			[{list, [{type, table}]}]
		end
	end,
    {Controller, NextState};

event_cb({endElement, "", Tag, {"", Tag}}, _Location, {Controller, State}) ->
    NextState =
	case {Tag, State} of
	    {"SESSION", []} ->
		eof;

	    {"LANGUAGE", [{language, _}=Language]} ->
		gen_server:call(Controller, Language),
		[];

	    {"LOGIN", [{login, _}=Login]} ->
		gen_server:call(Controller, Login),
		[];

	    {"NAME", [{name, _}=Param, {login, Params}]} ->
		[{login, Params ++ [Param]}];
	    {"PASSWORD", [{password, _}=Param, {login, Params}]} ->
		[{login, Params ++ [Param]}];
	    {"EMAIL", [{email, _}=Param, {login, Params}]} ->
		[{login, Params ++ [Param]}];

	    {"LIST", [{list, _}=List]} ->
		gen_server:call(Controller, List),
		[]

	end,
    {Controller, NextState};

event_cb({characters, Chars}, _Location, {Controller, [{Tag, String}|State]}) ->
    {Controller, [{Tag, String++Chars}|State]}.
