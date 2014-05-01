%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex main processing server
%%% @end
%%% Created :  3 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([reload/0, load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([output/2, output/3, input/2, event/2, transmit/2]).
-export([match_value/2, match_pattern/2]).


-include("../include/hex.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, hex_table).

-record(state, {
	  file = "" :: string(),
	  nodeid = 0 :: integer(),
	  tab :: ets:tab(),
	  out_list = []    :: [{Label::integer(), Pid::pid()}],
	  in_list  = []    :: [{Label::integer(), Pid::pid()}],
	  evt_list = []    :: [{Label::integer(), Ref::reference()}],
	  transmit_rules = []  :: [#hex_transmit{}],
	  input_rules = [] :: [#hex_input{}],
	  plugin_up = []   :: [{App::atom(), Mon::reference()}],
	  plugin_down = [] :: [App::atom()]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

reload() ->
    gen_server:call(?SERVER, reload).

load(File) ->
    gen_server:call(?SERVER, {load,File}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

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
init(Options) ->
    Tab = ets:new(?TABLE, [named_table]),
    Nodeid = proplists:get_value(nodeid, Options, 1),
    Name = proplists:get_value(config, Options, "local.conf"),
    File = case filename:dirname(Name) of
	       "." -> filename:join(code:priv_dir(hex), Name);
	       _ -> Name
	   end,
    lager:debug("starting hex_server nodeid=~.16B, config=~s",
		[Nodeid, File]),
    self() ! reload,
    {ok, #state{ nodeid = Nodeid,
		 file = File,
		 tab = Tab,
		 input_rules = []
	       }}.

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
handle_call({load,File}, _From, State) ->
    case reload(File, State) of
	{ok,State1} ->
	    {reply, ok, State1#state { file = File}};
	Error ->
	    {reply, Error, State}
    end;
handle_call(reload, _From, State) ->
    case reload(State#state.file, State) of
	{ok,State1} ->
	    {reply, ok, State1};
	Error ->
	    {reply, Error, State}
    end;
handle_call({join,Pid,AppName}, _From, State) when is_pid(Pid), 
						   is_atom(AppName) ->
    io:format("application ~w [~w] joined\n", [AppName,Pid]),
    AppMon = erlang:monitor(process,Pid),
    %% schedule load of event defintions for App in a while
    self() ! {init_plugin, AppName},
    PluginUp = [{AppName,AppMon} | State#state.plugin_up],
    PluginDown = lists:delete(AppName, State#state.plugin_down),
    {reply, ok, State#state { plugin_up = PluginUp, plugin_down = PluginDown }};
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_cast({event,Signal=#hex_signal{}}, State) ->
    lager:debug("input event: ~p\n", [Signal]),
    run_event(Signal, State#state.input_rules, State),
    {noreply, State};

handle_cast({transmit,Signal=#hex_signal{}}, State) ->
    lager:debug("transmit event: ~p\n", [Signal]),
    run_transmit(Signal, State#state.transmit_rules),
    {noreply, State};

handle_cast({join,Pid,AppName}, State) when is_pid(Pid), 
					    is_atom(AppName) ->
    io:format("application ~w [~w] joined\n", [AppName,Pid]),
    AppMon = erlang:monitor(process,Pid),
    %% schedule load of event defintions for App in a while
    self() ! {init_plugin, AppName},
    PluginUp = [{AppName,AppMon} | State#state.plugin_up],
    PluginDown = lists:delete(AppName, State#state.plugin_down),
    {noreply, State#state { plugin_up = PluginUp, plugin_down = PluginDown }};

handle_cast(_Cast, State) ->
    lager:debug("got cast: ~p", [_Cast]),
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
handle_info(reload, State) ->
    lager:debug("reload", []),
    case reload(State#state.file, State) of
	{ok,State1} ->
	    {noreply, State1};
	_Error ->
	    {noreply, State}
    end;

handle_info({init_plugin, AppName}, State) ->
    io:format("INIT PLUGIN: ~s\n", [AppName]),
    %% reload all events for Plugin AppName
    {noreply, State};

handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case lists:keytake(Ref, 2, State#state.plugin_up) of
	false ->
	    {noreply, State};
	{value,{App,_Ref},PluginUp} ->
	    io:format("PUGIN DOWN: ~s reason=~p\n", [App,_Reason]),
	    PluginDown = [App|State#state.plugin_down],
	    {noreply, State#state { plugin_up   = PluginUp, 
				    plugin_down = PluginDown }}
    end;
handle_info(_Info, State) ->
    lager:debug("got info: ~p", [_Info]),
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

reload(File, State) ->
    case file:consult(File) of
	{ok,Config} ->
	    case hex_config:scan(Config) of
		{ok,{Evt,In,Out,Trans}} ->
		    State1 = start_outputs(Out, State),
		    State2 = start_inputs(In, State1),
		    State3 = start_events(Evt, State2),
		    State4 = start_transmit(Trans, State3),
		    {ok, State4#state { input_rules = In }};
		Error={error,Reason} ->
		    io:format("error loading ~s\n~p\n", [File,Reason]),
		    Error
	    end;
	Error={error,Reason} ->
	    io:format("error loading ~s\n~p\n", [File,Reason]),
	    Error
    end.


start_outputs([#hex_output { label=L, flags=Flags, actions=Actions} | Out],
	      State) ->
    case Actions of
	{App,AppFlags} -> 
	    start_plugin(App,out,AppFlags);
	_ when is_list(Actions) ->
	    lists:foreach(
	      fun({_Pattern,{App,AppFlags}}) ->
		      start_plugin(App,out,AppFlags) 
	      end, Actions)
    end,
    %% nodeid and chan is needed for feedback from output
    Flags1 = [{nodeid,State#state.nodeid},{chan,L}|Flags],
    {ok,Pid} = hex_output:start_link(Flags1, Actions),
    ets:insert(State#state.tab, {{output,L}, Pid}),
    OutList = [{L,Pid} | State#state.out_list],
    start_outputs(Out, State#state { out_list = OutList });
start_outputs([], State) ->
    State.

start_inputs([#hex_input { label=L, flags = Flags } | In],
	     State) ->
    {ok,Pid} = hex_input:start_link(Flags),
    ets:insert(State#state.tab, {{input,L}, Pid}),
    InList = [{L,Pid} | State#state.in_list],
    start_inputs(In, State#state { in_list = InList });
start_inputs([], State) ->
    State.

%% start 
start_events([#hex_event { label=L,app=App,flags=Flags,signal=Signal } | Evt],
	     State) ->
    case start_plugin(App, in, Flags) of
	ok ->
	    lager:debug("add_event: ~p ~p", [App,Flags]),
	    {ok,Ref} = App:add_event(Flags, Signal, ?MODULE),
	    lager:debug("event ~w started ~w", [L, Ref]),
	    EvtList = [{L,Ref} | State#state.evt_list],
	    start_events(Evt, State#state { evt_list = EvtList });
	_Error ->
	    start_events(Evt, State)
    end;
start_events([], State) ->
    State.

start_transmit([T=#hex_transmit { app=App,flags=Flags } |
		Ts],   State) ->
    case start_plugin(App, out, Flags) of
	ok ->
	    Rules = [T | State#state.transmit_rules],
	    start_transmit(Ts, State#state { transmit_rules = Rules });
	_Error ->
	    start_transmit(Ts, State)
    end;
start_transmit([], State) ->
    State.
    

start_plugin(App, Dir, Flags) ->
    case hex:start_all(App) of
	{ok,[]} -> %% already started
	    init_plugin(App, Dir, Flags);
	{ok,Started} ->
	    lager:info("plugin ~w started: ~p", [App,Started]),
	    init_plugin(App, Dir, Flags);
	Error ->
	    lager:error("plugin ~w failed to start: ~p", [App, Error]),
	    Error
    end.

init_plugin(App, Dir, Flags) ->
    lager:debug("init_event: ~p ~p", [App,Flags]),
    case App:init_event(Dir, Flags) of
	ok ->
	    lager:info("~w:~w event ~p initiated", [App,Dir,Flags]),
	    ok;
	Error ->
	    lager:info("~w:~w event ~p failed ~p", [App,Dir,Flags,Error]),
	    Error
    end.


run_event(Signal, Rules, State) when is_record(Signal, hex_signal) ->
    run_event_(Signal, Rules, State),
    case Signal#hex_signal.type of
	?HEX_POWER_ON  -> run_power_on(Signal, Rules, State);
	%% ?HEX_POWER_OFF -> run_power_off(Signal, Rules, State);
	%% ?HEX_WAKEUP    -> run_wakeup(Signal, Rules, State);
	%% ?HEX_ALARM     -> run_alarm(Signal, Rules, State);
	_ -> ok
    end.

run_event_(Signal, [Rule|Rules], State) ->
    case match_pattern(Signal, Rule#hex_input.signal) of
	{true,Value} ->
	    Src = Signal#hex_signal.source,
	    V = case Signal#hex_signal.type of
		    ?HEX_DIGITAL -> {digital,Value,Src};
		    ?HEX_ANALOG ->  {analog,Value,Src};
		    ?HEX_ENCODER -> {encoder,Value,Src};
		    ?HEX_RFID    -> {rfid,Value,Src};
		    Type -> {Type,Value,Src}
		end,
	    input(Rule#hex_input.label, V),
	    run_event_(Signal, Rules, State);
	false ->
	    run_event_(Signal, Rules, State)
    end;
run_event_(_Signal, [], _State) ->
    ok.

run_power_on(Signal, [Rule|Rules], State) ->
    lager:debug("power on signal ~p", [Signal]),
    RulePattern = Rule#hex_input.signal,
    if is_integer(Signal#hex_signal.id),
       Signal#hex_signal.id =:= RulePattern#hex_pattern.id,
       is_integer(RulePattern#hex_pattern.chan) ->
	    add_outputs(Rule#hex_input.flags,
			RulePattern#hex_pattern.id,
			RulePattern#hex_pattern.chan,
			State),
	    run_power_on(Signal, Rules, State);
       true ->
	    run_power_on(Signal, Rules, State)
    end;
run_power_on(_Signal, [], _State) ->
    ok.

add_outputs([{output,Output}|Flags], Rid, Rchan, State) ->
    case proplists:get_value(channel,Output,0) of
	Chan when is_integer(Chan), Chan > 0, Chan =< 254 -> 
	    lager:debug("add output id=~.16B, chan=~w id=~.16B, rchan=~w",
		   [State#state.nodeid, Chan, Rid, Rchan]),
	    Value = (Rid bsl 8) bor Rchan,
	    Add = #hex_signal { id=make_self(State#state.nodeid),
				chan=Chan,
				type=?HEX_OUTPUT_ADD,
				value=Value,
				source={output,Chan}},
	    run_transmit(Add, State#state.transmit_rules),
	    add_outputs(Flags, Rid, Rchan, State);
	_ ->
	    add_outputs(Flags, Rid, Rchan, State)
    end;
add_outputs([_|Flags], Rid, Rchan, State) ->
    add_outputs(Flags, Rid, Rchan, State);    
add_outputs([], _Rid, _Rchan, _State) ->
    ok.

make_self(NodeID) ->
    if NodeID band 16#02000000 =/= 0 ->
	    16#20000000 bor (2#0011 bsl 25) bor (NodeID band 16#1ffffff);
       true ->
	    (2#0011 bsl 9) bor (NodeID band 16#7f)
    end.
    

%% handle "distribution" messages when match
run_transmit(Signal, Rules) when is_record(Signal, hex_signal) ->
    run_transmit_(Signal, Rules).

run_transmit_(Signal, [Rule|Rules]) ->
    case match_pattern(Signal, Rule#hex_transmit.signal) of
	{true,_Value} ->
	    App = Rule#hex_transmit.app,
	    App:transmit(Signal, Rule#hex_transmit.flags),
	    run_transmit_(Signal, Rules);
	false ->
	    run_transmit_(Signal, Rules)
    end;
run_transmit_(_Signal, []) ->
    ok.



match_pattern(Sig, Pat) when is_record(Sig, hex_signal),
			     is_record(Pat, hex_pattern) ->
    case match_value(Pat#hex_pattern.id,Sig#hex_signal.id) andalso 
	match_value(Pat#hex_pattern.chan,Sig#hex_signal.chan) andalso 
	match_value(Pat#hex_pattern.type, Sig#hex_signal.type) of
	true ->
	    case match_value(Pat#hex_pattern.value,Sig#hex_signal.value) of
		true -> {true, Sig#hex_signal.value};
		false -> false
	    end;
	false ->
	    false
    end.

match_value(A, A) -> true;
match_value({mask,Mask,Match}, A) -> A band Mask =:= Match;
match_value({range,Low,High}, A) -> (A >= Low) andalso (A =< High);
match_value({'not',Cond}, A) -> not match_value(Cond,A);
match_value({'and',C1,C2}, A) -> match_value(C1,A) andalso match_value(C2,A);
match_value({'or',C1,C2}, A) -> match_value(C1,A) orelse match_value(C2,A);
match_value([Cond|Cs], A) -> match_value(Cond,A) andalso match_value(Cs, A);
match_value([], _A) -> true;
match_value(_, _) -> false.

input(I, Value) when is_integer(I); is_atom(I) ->
    lager:debug("input ~w ~w", [I, Value]),
    try ets:lookup(?TABLE, {input,I}) of
	[] ->
	    lager:warning("hex_input ~w not running", [I]),
	    ignore;
	[{_,Fsm}] ->
	    gen_fsm:send_event(Fsm, Value)
    catch 
	error:badarg ->
	    lager:warning("hex_server not running", []),
	    ignore
    end.

%% called from hex_input !
output(O, Value) ->
    output(O,value,Value).

output(O,Channel,Value={Type,_,_}) when 
      is_atom(Channel), is_integer(O), O >= 1, O =< 254 ->
    lager:debug("output ~w:~s ~w", [O,Channel,Value]),
    try ets:lookup(?TABLE, {output,O}) of
	[] -> 
	    lager:warning("output ~w not running", [O]),
	    ignore;
	[{_,Fsm}] ->
	    if Channel =:= value, Type =:= digital ->
		    gen_fsm:send_event(Fsm, Value);
	       true ->
		    Fsm ! {Channel,Value}  %% end up in handle_info
	    end
    catch
	error:badarg ->
	    lager:warning("hex_server not running", []),
	    ignore
    end.

transmit(Signal=#hex_signal{}, _Env) ->
    gen_server:cast(?SERVER, {transmit, Signal}).

%% generate input event to dispatcher
event(S0=#hex_signal{}, Env) ->
    S = #hex_signal { id    = event_value(S0#hex_signal.id, Env),
		      chan  = event_value(S0#hex_signal.chan, Env),
		      type  = event_value(S0#hex_signal.type, Env),
		      value = event_value(S0#hex_signal.value, Env),
		      source = event_value(S0#hex_signal.source, Env)
		    },
    gen_server:cast(?SERVER, {event, S}).

event_value(Var, Env) when is_atom(Var) ->
    proplists:get_value(Var, Env, 0);
event_value(Value, _Env) ->
    Value.

