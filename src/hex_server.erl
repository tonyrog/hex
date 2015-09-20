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
-export([reload/0, 
	 load/1, 
	 dump/0]).
-export([subscribe/1,
	 unsubscribe/0]).
-export([inform/2]).
-export([event_list/0]).
-export([event_signal/1]).
-export([input_active/2]).
-export([input2outputs/1]).
-export([output2pid/1]).
-export([input2output_pids/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-export([output/3, 
	 input/2, 
	 event/2, 
	 event_and_transmit/2, 
	 analog_event_and_transmit/2, 
	 transmit/2]).
-export([match_value/2, 
	 match_pattern/2,
	 match_pattern/3,
	 match_bin_pattern/3
	]).

-include("../include/hex.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, hex_table).

-type config() :: term().

-record(map_item,
	{
	  label         :: integer() | atom(),          
	  id            :: uint32(),  %% remote id
	  channel       :: uint8()   %% remote channel number
	}).

-record(int_event,
	{
	  label          :: integer() | atom(),          
	  ref            :: reference(),
	  app            :: atom(),
	  app_flags      :: [{Key::atom(), Value::term()}],
	  signal         :: #hex_signal{},
	  alarm=0        :: integer(), %% alarm id (0=ok)
	  active=false   :: boolean()  %% status of id:chan 
	}).

-record(subscriber,
	{
	  pid  :: pid(),
	  mon  :: reference(),
	  options=[] :: [{Key::atom(), Value::term()}]
	}).
		  
-record(state, {
	  config = default :: default | {file,string()} | [config()],
	  nodeid = 0 :: integer(),
	  tab :: ets:tab(),
	  out_list = []    :: [{Label::integer(), Pid::pid()}],
	  in_list  = []    :: [{Label::integer(), Pid::pid()}],
	  evt_list = []    :: [#int_event{}],
	  map = []         :: [#map_item{}],
	  transmit_rules = []  :: [#hex_transmit{}],
	  input_rules = [] :: [#hex_input{}],
	  plugin_up = []   :: [{App::atom(), Mon::reference()}],
	  plugin_down = [] :: [App::atom()],
	  subs = []        :: [#subscriber{}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

reload() ->
    gen_server:call(?SERVER, reload).

load(File) ->
    gen_server:call(?SERVER, {load, File}).

dump() ->
    gen_server:call(?SERVER, dump).

subscribe(Options) ->
    gen_server:call(?SERVER, {subscribe, self(), Options}).

unsubscribe() ->
    gen_server:call(?SERVER, {unsubscribe, self()}).

inform(Type, Options) ->
    ?SERVER ! {inform, Type, Options}.

event_list() ->
    gen_server:call(?SERVER, event_list).
    
event_signal(Label) ->
    gen_server:call(?SERVER, {event_signal, Label}).
    
input_active(Label, Active) ->
    gen_server:call(?SERVER, {input_active, Label, Active}).
    
input2outputs(Label) ->
    gen_server:call(?SERVER, {input2outputs, Label}).

output2pid(Channel) ->
    gen_server:call(?SERVER, {output2pid, Channel}).

input2output_pids(Label) ->
    gen_server:call(?SERVER, {input2output_pids, Label}).

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
    Config =
	case proplists:get_value(config, Options, default) of
	    default ->
		{file, "local.conf"};
	    ConfigOpt ->
		case hex:is_string(ConfigOpt) of
		    true -> {file, hex:text_expand(ConfigOpt,[])};
		    false -> ConfigOpt
		end
	end,
    Map = create_map(proplists:get_value(map, Options, []),[]),

    lager:debug("starting hex_server nodeid=~.16B, config=~p",
		[Nodeid, Config]),
    self() ! reload,
    {ok, #state{ nodeid = Nodeid,
		 config = Config,
		 map = Map,
		 tab = Tab,
		 input_rules = []
	       }}.

create_map([], Acc) ->
    Acc;
create_map([{Label, CobId, Chan} | Rest], Acc) ->
    create_map(Rest, [#map_item {label = Label,
				 id = hex_config:pattern(CobId),
				 channel = Chan} | Acc]).

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
    case hex:is_string(File) of
	true ->
	    File1 = hex:text_expand(File,[]),
	    case reload(File1, State) of
		{ok,State1} ->
		    {reply, ok, State1#state { config={file,File1}}};
		Error ->
		    {reply, Error, State}
	    end;
	false ->
	    {reply, {error,einval}, State}
    end;
handle_call(reload, _From, State) ->
    case reload(State#state.config, State) of
	{ok,State1} ->
	    {reply, ok, State1};
	Error ->
	    {reply, Error, State}
    end;
handle_call({join,Pid,AppName}, _From, State) when is_pid(Pid),
						   is_atom(AppName) ->
    lager:info("plugin ~s [~w] joined", [AppName,Pid]),
    AppMon = erlang:monitor(process,Pid),
    %% schedule load of event defintions for App in a while
    self() ! {init_plugin, AppName},
    PluginUp = [{AppName,AppMon} | State#state.plugin_up],
    PluginDown = lists:delete(AppName, State#state.plugin_down),
    {reply, ok, State#state { plugin_up = PluginUp, plugin_down = PluginDown }};
handle_call({subscribe, Pid, Options}, _From, State=#state {subs = Subs}) ->
    case lists:keyfind(Pid, #subscriber.pid, Subs) of
	S when is_record(S, subscriber) ->
	    {reply, ok, State};
	false ->
	    Mon = erlang:monitor(process, Pid),
	    Sub = #subscriber {pid = Pid, mon = Mon, options = Options},
	    {reply, ok, State#state {subs = [Sub | Subs]}}
    end;
handle_call({unsubscribe, Pid}, _From, State=#state {subs = Subs}) ->
    case lists:keytake(Pid, #subscriber.pid, Subs) of
	false ->
	    {reply, ok, State};
	{value, #subscriber {mon = Mon}, NewSubs} ->
	    erlang:demonitor(Mon, [flush]),
	    {reply, ok, State#state {subs = NewSubs}}
    end;
handle_call({inform, Type, Options}, _From, State=#state {subs = Subs }) ->
    inform_subscribers({Type, Options}, Subs),
    {reply, ok, State};
    
handle_call(event_list, _From, State=#state {evt_list = EList}) ->
    List = [{E#int_event.label, E#int_event.signal} || E <- EList],
    {reply, {ok, List}, State};
handle_call({event_signal, Label}, _From, State=#state {evt_list = EList}) ->
    case lists:keyfind(Label, #int_event.label, EList) of
	#int_event {signal = Signal} ->
	    {reply, {ok, Signal}, State};
	false ->
	    lager:debug("Unknown event ~p", [Label]),
	    {reply, {error, unknown_event}, State}
    end;
handle_call({input_active, Label, Active}, _From, 
	    State=#state {input_rules = IList}) ->
    lager:debug("input_active: ~p to ~p\n", [Label, Active]),
    case lists:keytake(Label, #hex_input.label, IList) of
	{value, I=#hex_input {flags = Flags}, Rest} ->
	    case lists:keyfind(active, 1, Flags) of
		{active, Active} ->
		    %% no change
		    {reply, ok, State};
		{active, _Other} ->
		    NewFlags = 
			lists:keyreplace(active, 1, Flags, {active, Active}),
		    NewI = I#hex_input {flags = NewFlags},
		    {reply, ok, State#state {input_rules = [NewI | Rest]}};
		false ->
		    NewI = I#hex_input {flags = [{active, Active} | Flags]},
		    {reply, ok, State#state {input_rules = [NewI | Rest]}}
	    end;
	false ->
	    lager:debug("Unknown label ~p", [Label]),
	    {reply, {error, unknown_input}, State}
    end;
handle_call({input2outputs, Label}, _From, 
	    State=#state {input_rules = IList}) ->
    lager:debug("input2outputs: ~p\n", [Label]),
    {reply, input2outputs(Label, IList), State};
handle_call({output2pid, Channel}, _From, 
	    State=#state {out_list = OList}) ->
    lager:debug("output2pid: ~p\n", [Channel]),
    {reply, output2pid(Channel, OList), State};
handle_call({input2output_pids, Label}, _From, 
	    State=#state {input_rules = IList, out_list = OList}) ->
    lager:debug("input2output_pids: ~p\n", [Label]),
    case input2outputs(Label, IList) of
	List when is_list(List) ->
	    Reply = [output2pid(V, OList) || {K,V} <- List, K =:= channel],
	    {reply, Reply, State};
	{error, _Reason} = E -> {reply, E, State}
    end;
handle_call({event_and_transmit, Label, Value}, _From, 
	    State=#state {evt_list = EList}) ->
    lager:debug("event_and_transmit: ~p\n", [Label]),
    case {lists:keyfind(Label, #int_event.label, EList), Value} of
	{#int_event {signal = (Signal=#hex_signal {type = ?HEX_DIGITAL}), 
		     alarm = Alarm}, 1}
	  when Alarm =/= 0 ->
	    Signal1 = Signal#hex_signal {value = Value},
	    alarm_confirm(Label, Signal1, State),
	    NewState = run_event(Signal1, <<>>, State#state.input_rules, State),
	    {reply, ok, NewState};
	{#int_event {signal = Signal},_} ->
	    Signal1 = Signal#hex_signal {value = Value},
	    run_transmit(Signal1, State#state.transmit_rules),
	    NewState = run_event(Signal1, <<>>, State#state.input_rules, State),
	    {reply, ok, NewState};
	{false, _} ->
	    lager:debug("Unknown event ~p", [Label]),
	    {reply, {error, unknown_event}, State}
    end;
handle_call({analog_event_and_transmit, Label, Value}, _From, 
	    State=#state {evt_list = EList}) ->
    lager:debug("event_and_transmit: ~p\n", [Label]),
    case {lists:keyfind(Label, #int_event.label, EList), Value} of
	{#int_event {signal = (Signal=#hex_signal {type = ?HEX_DIGITAL}), 
		     alarm = Alarm}, 1}
	  when Alarm =/= 0 ->
	    Signal1 = Signal#hex_signal {value = Value, type = ?HEX_ANALOG},
	    alarm_confirm(Label, Signal1, State),
	    NewState = run_event(Signal1, <<>>, State#state.input_rules, State),
	    {reply, ok, NewState};
	{#int_event {signal = Signal},_} ->
	    Signal1 = Signal#hex_signal {value = Value, type = ?HEX_ANALOG},
	    run_transmit(Signal1, State#state.transmit_rules),
	    NewState = run_event(Signal1, <<>>, State#state.input_rules, State),
	    {reply, ok, NewState};
	{false, _} ->
	    lager:debug("Unknown event ~p", [Label]),
	    {reply, {error, unknown_event}, State}
    end;
handle_call(dump, _From, State) ->
    io:format("State ~p", [State]),
    {reply, ok, State};
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
handle_cast({event,Signal=#hex_signal{},Data}, State) ->
    lager:debug("event: ~p data=~p\n", [Signal,Data]),
    NewState = run_event(Signal, Data, State#state.input_rules, State),
    {noreply, NewState};

handle_cast({transmit,Signal=#hex_signal{}}, State) ->
    lager:debug("transmit: ~p\n", [Signal]),
    run_transmit(Signal, State#state.transmit_rules),
    {noreply, State};

handle_cast({join,Pid,AppName}, State) when is_pid(Pid),
					    is_atom(AppName) ->
    lager:info("plugin ~s [~w] joined", [AppName,Pid]),
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
    case reload(State#state.config, State) of
	{ok,State1} ->
	    {noreply, State1};
	_Error ->
	    {noreply, State}
    end;

handle_info({init_plugin, AppName}, State) ->
    lager:debug("init PLUGIN: ~s", [AppName]),
    %% reload all events for Plugin AppName
    {noreply, State};

handle_info({inform, Type, Options}, State=#state {subs = Subs }) ->
    lager:debug("inform: ~p ~p", [Type, Options]),
    inform_subscribers({Type, Options}, Subs),
    {noreply, State};

handle_info({'DOWN',Ref,process,Pid,_Reason}, State) ->
    case lists:keytake(Ref, 2, State#state.plugin_up) of
	false ->
	    case lists:keytake(Ref, #subscriber.mon, State#state.subs) of
		false ->
		    {noreply, State};
		{value, #subscriber {pid = Pid}, NewSubs} ->
		    lager:warning("subscriber DOWN: ~p reason=~p", 
				  [Pid,_Reason]),
		    {noreply, State#state { subs = NewSubs}}
	    end;
	{value,{App,_Ref},PluginUp} ->
	    lager:warning("plugin DOWN: ~s reason=~p", [App,_Reason]),
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

reload({file,File}, State) ->
    case file:consult(File) of
	{ok,Config} ->
	    rescan(Config, File, State);
	Error={error,Reason} ->
	    io:format("~s: file error:  ~p\n", [File,Reason]),
	    lager:error("error loading file ~s\n~p", [File,Reason]),
	    Error
    end;
reload(Config, State) ->
    rescan(Config, "*config*", State).


rescan(Config, File, State) ->
    case hex_config:scan(Config) of
	{ok,{Evt,In,Out,Trans}} ->
	    State1 = start_outputs(Out, State),
	    State2 = start_inputs(In, State1),
	    State3 = start_events(Evt, State2),
	    State4 = start_transmit(Trans, State3),
	    {ok, State4#state { input_rules = In }};
	Error={error,Reason} ->
	    io:format("config error ~p\n", [Reason]),
	    lager:error("error config file ~s, ~p", [File,Reason]),
	    Error
    end.


start_outputs([#hex_output { label=L, flags=Flags, actions=Actions} | Out],
	      State) ->
    case Actions of
	{App,AppFlags} ->
	    lager:debug("start_output: ~p ~p", [App,AppFlags]),
	    start_plugin(App,out,AppFlags);
	_ when is_list(Actions) ->
	    lists:foreach(
	      fun({_Pattern,{App,AppFlags}}) ->
		      lager:debug("start_output: ~p ~p", [App,AppFlags]),
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
    {ok,Pid} = hex_input:start_link([{id,L}|Flags]),
    ets:insert(State#state.tab, {{input,L}, Pid}),
    InList = [{L,Pid} | State#state.in_list],
    start_inputs(In, State#state { in_list = InList });
start_inputs([], State) ->
    State.

%% start
start_events([#hex_event { label=L,app=App,app_flags=AppFlags,signal=Signal } | Evt],
	     State) ->
    lager:debug("start_plugin: ~p ~p", [App,AppFlags]),
    case start_plugin(App, in, AppFlags) of
	ok ->
	    case App:add_event(AppFlags, Signal, ?MODULE) of
		{ok,Ref} ->
		    lager:debug("event ~w started ~w", [L, Ref]),
		    EvtList = [#int_event{ label = L, ref = Ref, app = App,
					   app_flags = AppFlags, 
					   signal = Signal} | 
			       State#state.evt_list],
		    start_events(Evt, State#state { evt_list = EvtList });
		Error ->
		    lager:error("unable to add_event: ~p ~p: ~p",
				[App,AppFlags,Error]),
		    start_events(Evt, State)
	    end;
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

run_event(Signal, Data, Rules, State) 
  when is_record(Signal, hex_signal) ->
    run_event_(Signal, Data, Rules, State),
    case Signal#hex_signal.type of
	?HEX_POWER_ON     -> run_power_on(Signal, Rules, State);
	?HEX_OUTPUT_ADD   -> run_output_add(Signal, State);
        ?HEX_OUTPUT_DEL   -> run_output_del(Signal, State);
        ?HEX_OUTPUT_ACTIVE -> run_output_act(Signal, State);
	%% ?HEX_POWER_OFF -> run_power_off(Signal, Rules, State);
	%% ?HEX_WAKEUP    -> run_wakeup(Signal, Rules, State);
	?HEX_ALARM        -> run_alarm(Signal, State);
	?HEX_ALARM_CNFRM_ACK -> run_alarm_confirm_ack(Signal, State);
	_ -> State
    end.

run_event_(Signal, Data, [Rule|Rules], State) ->
    case match_pattern(Signal, Data, Rule#hex_input.signal) of
	{true, Type, Value} ->
	    case active(Rule#hex_input.flags) of
		true ->
		    Src = Signal#hex_signal.source,
		    V = {Type, Value, Src},
		    input(Rule#hex_input.label, V),
		    run_event_(Signal, Data, Rules, State);
		false ->
		    run_event_(Signal, Data, Rules, State)
	    end;
	false ->
	    run_event_(Signal, Data, Rules, State)
    end;
run_event_(_Signal, _Data, [], _State) ->
    ok.

active([]) -> true;
active([{active, true} | _Flags]) -> true;
active([{active, false} | _Flags]) -> false;
active([_Flag | Flags]) -> active(Flags).
    

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
run_power_on(_Signal, [], State) ->
    State.

add_outputs([{output,Output}|Flags], Rid, Rchan, State) ->
    case proplists:get_value(channel,Output,0) of
	Chan when is_integer(Chan), Chan > 0, Chan =< 254 ->
	    lager:debug("add output id=~.16B, chan=~w id=~.16B, rchan=~w",
		   [State#state.nodeid, Chan, Rid, Rchan]),
	    Value = (Rid bsl 8) bor Rchan,
	    Add = #hex_signal { id=hex:make_self(State#state.nodeid),
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


run_output_add(Signal=#hex_signal {value = Value}, 
	       State=#state {evt_list = EvtList}) ->
    lager:debug("run_output_add: ~p", [Signal]),
    {Id, Chan} = value2nid(Value),
    lager:debug("run_output_add: ~.16B ~p", [Id, Chan]),
    if Chan >=1, Chan =< 254 -> 
	    run_output_add(Id, Chan, Signal, EvtList, State);
       true ->
	    State
    end.

run_output_add(Id, LChan, Signal=#hex_signal {id = RId, chan = RChan}, 
	       [#int_event {label = Label, signal = S} | Events], 
	       State=#state{map = Map}) ->
    if is_integer(S#hex_signal.id) ->
	    SigNodeId = S#hex_signal.id band 
		(?HEX_XNODE_ID_MASK bor ?HEX_COBID_EXT),
	    if Id =:= SigNodeId,
	       LChan =:= S#hex_signal.chan ->
		    case is_mapped(Map, Label, RId, RChan) of
			true ->
			    run_output_add(Id, LChan, Signal, Events, State);
			false ->
			    MapItem = #map_item{label = Label,
						id = RId,
						channel = RChan},
			    lager:debug("run_output_add: item ~p", [MapItem]),
			    run_output_add(Id, LChan, Signal, Events, 
					   State#state {map = [MapItem | Map]})
		    end;
	       true ->
		    run_output_add(Id, LChan, Signal, Events, State)
	    end;
       true ->
	    run_output_add(Id, LChan, Signal, Events, State)
    end;
run_output_add(_Id, _LChan, _Signal, [], State) ->
    State.


run_output_del(Signal=#hex_signal {value = Value}, 
	       State=#state {evt_list = EvtList}) ->
    lager:debug("run_output_del: ~p", [Signal]),
    {Id, Chan} = value2nid(Value),
    lager:debug("run_output_del: ~.16B ~p", [Id, Chan]),
    if Chan >=1, Chan =< 254 -> 
	    run_output_del(Id, Chan, Signal, EvtList, State);
       true ->
	    State
    end.

run_output_del(Id, LChan, Signal=#hex_signal {id = RId, chan = RChan}, 
	       [#int_event {label = Label, signal = S} | Events], 
	       State=#state{map = Map}) ->
    if is_integer(S#hex_signal.id) ->
	    SigNodeId = S#hex_signal.id band 
		(?HEX_XNODE_ID_MASK bor ?HEX_COBID_EXT),
	    
	    if Id =:= SigNodeId,
	       LChan =:= S#hex_signal.chan ->
		    case is_mapped(Map, Label, RId, RChan) of
			true ->
			    lager:debug("run_output_del: item ~p, ~p, ~p", 
					[Label, RId, RChan]),
			    NewMap = remove_mapped(Label, RId, RChan, Map, []),
			    run_output_del(Id, LChan, Signal, Events, 
					   State#state {map = NewMap});
			false ->
			    run_output_del(Id, LChan, Signal, Events, State)
		    end;
	       true ->
		    run_output_del(Id, LChan, Signal, Events, State)
	    end;
       true ->
	    run_output_del(Id, LChan, Signal, Events, State)
    end;
run_output_del(_Id, _LChan, _Signal, [], State) ->
    State.

run_output_act(Signal=#hex_signal {id = Id, chan = Chan, value = Value}, 
	       State=#state {map = Map}) ->
    lager:debug("signal ~p", [Signal]),
    %%Id = Id0 band (?HEX_XNODE_ID_MASK bor ?HEX_COBID_EXT),
    run_output_act(Id, Chan, Value, Map, State).

run_output_act(Id, Chan, Active, 
	       [#map_item {label = Label, id = Id, channel = Chan} | Map], 
	       State=#state{evt_list = EList, subs = Subs}) ->
    lager:debug("event ~p, active ~p", [Label, Active]),
    NewElist = event_active(Label, Active, EList, [], Subs),
    run_output_act(Id, Chan, Active, Map, State#state{evt_list = NewElist});
run_output_act(Id, Chan, Active, [_MapItem | Map], State) ->
    run_output_act(Id, Chan, Active, Map, State);
run_output_act(_Id, _Chan, _Active, [], State) ->
    State.

event_active(_Label, _Active, [], Acc, _Subs) ->
    Acc;
event_active(Label, Active, 
	     [E=#int_event {label = Label, app = App, app_flags = AppFlags} | 
	      Events], 
	     Acc, Subs) ->
    App:output(AppFlags, [{output_active, Active}]),
    inform_subscribers({'output-active', [{label, Label}, {value, Active}]}, Subs),
    event_active(Label, Active, Events, 
		 [E#int_event {active = (Active =/= 0)} | Acc], Subs);
event_active(Label, Active, [E | Events], Acc, Subs) ->
    event_active(Label, Active, Events, [E | Acc], Subs).

run_alarm(Signal=#hex_signal {id = Id, chan = Chan, value = Value}, 
	       State=#state {map = Map}) ->
    lager:debug("signal ~p", [Signal]),
    run_alarm(Id, Chan, Value, Map, State).

run_alarm(Id, Chan, Alarm, 
	       [#map_item {label = Label, id = Id, channel = Chan} | Map], 
	       State=#state{evt_list = EList, subs = Subs}) ->
    lager:debug("event ~p, alarm ~p", [Label, Alarm]),
    NewElist = event_alarm(Label, Alarm, EList, [], Subs),
    run_alarm(Id, Chan, Alarm, Map, State#state{evt_list = NewElist});
run_alarm(Id, Chan, Alarm, [_MapItem | Map], State) ->
    run_alarm(Id, Chan, Alarm, Map, State);
run_alarm(_Id, _Chan, _Alarm, [], State) ->
    State.

event_alarm(_Label, _Alarm, [], Acc, _Subs) ->
    Acc;
event_alarm(Label, Alarm, 
	    [E=#int_event {label = Label, app = App, app_flags = AppFlags} | 
	     Events], 
	    Acc, Subs) ->
    App:output(AppFlags, [{alarm, Alarm}]),
    inform_subscribers({alarm, [{label, Label}, {value, Alarm}]}, Subs),
    event_alarm(Label, Alarm, Events, [E#int_event {alarm = Alarm} | Acc], Subs);
event_alarm(Label, Alarm, [E | Events], Acc, Subs) ->
    event_alarm(Label, Alarm, Events, [E | Acc], Subs).


alarm_confirm(Label, #hex_signal{id = Id, chan = Chan}, State) ->
    Confirm = #hex_signal {id=Id,
			   chan=Chan,
			   type=?HEX_ALARM_CNFRM,
			   value=0,
			   source={event,Label}},
    run_transmit(Confirm, State#state.transmit_rules).


run_alarm_confirm_ack(Signal=#hex_signal {id = Id, chan = Chan, value = Value}, 
	       State=#state {map = Map}) ->
    lager:debug("signal ~p", [Signal]),
    run_alarm_confirm_ack(Id, Chan, Value, Map, State).

run_alarm_confirm_ack(Id, Chan, Alarm, 
	       [#map_item {label = Label, id = Id, channel = Chan} | Map], 
	       State=#state{evt_list = EList, subs = Subs}) ->
    lager:debug("event ~p, alarm ~p", [Label, Alarm]),
    NewElist = event_alarm_confirm_ack(Label, Alarm, EList, [], Subs),
    run_alarm_confirm_ack(Id, Chan, Alarm, Map, State#state{evt_list = NewElist});
run_alarm_confirm_ack(Id, Chan, Alarm, [_MapItem | Map], State) ->
    run_alarm_confirm_ack(Id, Chan, Alarm, Map, State);
run_alarm_confirm_ack(_Id, _Chan, _Alarm, [], State) ->
    State.

event_alarm_confirm_ack(_Label, _Alarm, [], Acc, _Subs) ->
    Acc;
event_alarm_confirm_ack(Label, Alarm, 
	    [E=#int_event {label = Label, app = App, app_flags = AppFlags} | 
	     Events], 
	    Acc, Subs) ->
    App:output(AppFlags, [{alarm_ack, 0}]),
    inform_subscribers({alarm_ack, [{label, Label}]}, Subs),
    event_alarm_confirm_ack(Label, Alarm, Events, [E#int_event {alarm = 0} | Acc], Subs);
event_alarm_confirm_ack(Label, Alarm, [E | Events], Acc, Subs) ->
    event_alarm_confirm_ack(Label, Alarm, Events, [E | Acc], Subs).

inform_subscribers(_Msg, []) ->
    ok;
inform_subscribers(Msg, [#subscriber {pid = Pid} | Subs]) ->
    lager:debug("informing ~p of ~p", [Pid, Msg]),
    Pid ! Msg,
    inform_subscribers(Msg, Subs).
    
value2nid(Value) ->
    Id0 = Value bsr 8,
    Id  = if Id0 < 127 -> Id0;
	      true -> Id0 bor ?HEX_COBID_EXT
	   end,
    Chan = (Value band 16#ff),
    {Id, Chan}.

is_mapped([#map_item{label=Label,id=Id,channel=Chan}|_Map],Label,Id,Chan) ->
    true;
is_mapped([_|Map],Label,Id,Chan) ->
    is_mapped(Map,Label,Id,Chan);
is_mapped([],_Label,_Id,_Chan) ->
    false.

remove_mapped(_Label,_Id,_Chan,[],Acc) ->
    Acc;
remove_mapped(Label,Id,Chan, 
	      [#map_item{label=Label,id=Id,channel=Chan}|Map],Acc) ->
    remove_mapped(Label,Id,Chan,Map,Acc);
remove_mapped(Label,Id,Chan,[MapItem|Map],Acc) ->
    remove_mapped(Label,Id,Chan,Map,[MapItem|Acc]).

%% handle "distribution" messages when match
run_transmit(Signal, Rules) when is_record(Signal, hex_signal) ->
    run_transmit_(Signal, Rules).

run_transmit_(Signal, [Rule|Rules]) ->
    case match_pattern(Signal, <<>>, Rule#hex_transmit.signal) of
	{true,_Type,_Value} ->
	    App = Rule#hex_transmit.app,
	    App:transmit(Signal, Rule#hex_transmit.flags),
	    run_transmit_(Signal, Rules);
	false ->
	    run_transmit_(Signal, Rules)
    end;
run_transmit_(_Signal, []) ->
    ok.

match_pattern(Sig, Pat) ->
    match_pattern(Sig, <<>>, Pat).

match_pattern(Sig, _Data, Pat) when is_record(Sig, hex_signal),
				    is_record(Pat, hex_pattern) ->
    case match_value(Pat#hex_pattern.id,Sig#hex_signal.id) andalso
	match_value(Pat#hex_pattern.chan,Sig#hex_signal.chan) andalso
	match_value(Pat#hex_pattern.type, Sig#hex_signal.type) of
	true ->
	    case match_value(Pat#hex_pattern.value,Sig#hex_signal.value) of
		true ->
		    case Sig#hex_signal.type of
			?HEX_DIGITAL ->
			    {true, digital, Sig#hex_signal.value};
			?HEX_ANALOG ->
			    {true, analog, Sig#hex_signal.value};
			?HEX_ENCODER ->
			    {true, encoder, Sig#hex_signal.value};
			?HEX_RFID    ->
			    {true, rfid, Sig#hex_signal.value};
			Type         ->
			    {true, Type, Sig#hex_signal.value}
		    end;
		false -> false
	    end;
	false ->
	    false
    end;
match_pattern(Sig, Data, Pat) when is_record(Sig, hex_signal),
				   is_binary(Data),
				   is_record(Pat, hex_bin_pattern) ->
    case match_value(Pat#hex_bin_pattern.id, Sig#hex_signal.id) of
	true ->
	    R = match_bin_pattern(Data, Pat#hex_bin_pattern.bin, []),
	    lager:debug("match bin pattern ~p / ~p = ~p\n",
			[Data, Pat#hex_bin_pattern.bin, R]),
	    R;
	false ->
	    false
    end.

match_bin_pattern(Data, [{Size,Bind}|Pattern], Bound) when is_integer(Size) ->
    case Data of
	<<Bind:Size/little,Data1/bitstring>> when is_integer(Bind) ->
	    match_bin_pattern(Data1, Pattern, Bound);
	<<_:Size/little,Data1/bitstring>> when Bind =:= '_' ->  %% skip
	    match_bin_pattern(Data1, Pattern, Bound);
	<<Value:Size/little,Data1/bitstring>> when is_atom(Bind) ->
	    match_bin_pattern(Data1, Pattern, [{Size,Bind,Value}|Bound]);
	_ ->
	    false
    end;
match_bin_pattern(_Data, [], Bound) ->  %% fixme keep bound name in env!
    case lists:reverse(Bound) of
	[{1,_Name,Value}|_] ->
	    {true, digital, Value};
	[{_Size,_Name,Value}|_] ->
	    {true, analog, Value}
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
    %% io:format("hex_server: INPUT ~w ~p\n", [I, Value]),
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

output(Channel,Target,Value={_Type,_,_}) when
      is_atom(Target), is_integer(Channel), Channel >= 1, Channel =< 254 ->
    lager:debug("output ~w:~s ~w", [Channel,Target,Value]),
    %% io:format("hex_server: OUTPUT  ~w:~s ~p\n", [Channel,Target,Value]),
    try ets:lookup(?TABLE, {output,Channel}) of
	[] ->
	    lager:warning("output ~w not running", [Channel]),
	    ignore;
	[{_,Fsm}] ->
	    gen_fsm:send_event(Fsm, {Target,Value})
    catch
	error:badarg ->
	    lager:warning("hex_server not running", []),
	    ignore
    end.

input2outputs(Label, List) ->
    case lists:keyfind(Label, #hex_input.label, List) of
	#hex_input {flags = Flags} ->
	    case lists:keyfind(output, 1, Flags) of
		{output, Output} ->
		    Output;
		false ->
		    {error, no_output}
	    end;
	false ->
	    {error, unknown_input}
    end.

output2pid(Channel, List) ->
    case lists:keyfind(Channel, 1, List) of
	{Channel, Pid} when is_pid(Pid) ->
	    Pid;
	false ->
	    {error, no_output_process}
    end.

transmit(Signal=#hex_signal{}, _Env) ->
    gen_server:cast(?SERVER, {transmit, Signal}).

event(S0=#hex_signal{}, Env) ->
    S = #hex_signal { id    = event_value(S0#hex_signal.id, Env),
		      chan  = event_value(S0#hex_signal.chan, Env),
		      type  = event_value(S0#hex_signal.type, Env),
		      value = event_value(S0#hex_signal.value, Env),
		      source = event_value(S0#hex_signal.source, Env)
		    },
    Data = proplists:get_value(data, Env, <<>>),
    gen_server:cast(?SERVER, {event, S, Data}).

event_and_transmit(Label, Value) ->
    gen_server:call(?SERVER, {event_and_transmit, Label, Value}).

analog_event_and_transmit(Label, Value) ->
    gen_server:call(?SERVER, {analog_event_and_transmit, Label, Value}).

event_value(Var, Env) when is_atom(Var) ->
    proplists:get_value(Var, Env, 0);
event_value(Value, _Env) ->
    Value.
