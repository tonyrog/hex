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
%%%    Hex output machine
%%% @end
%%% Created :  3 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_output).

-behaviour(gen_fsm).

-include("../include/hex.hrl").
%% API
-export([start_link/2]).
-export([validate_flags/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([state_off/2, 
	 state_on/2,
	 state_delay/2,
	 state_sustain/2,
	 state_wait/2,
	 state_rampup/2,
	 state_rampdown/2,
	 state_deact/2]).
	 
-define(SERVER, ?MODULE).

%% note!  a short pulse must be at least 1 milli long, since 
%% sustain = 0 means that output goes into state_on.
%%
-record(opt, {
	  nodeid  = 0 :: uint32(),    %% id of hex node
	  chan    = 0 :: uint8(),     %% output number 1..254	  
	  default = 0 :: uint32(),    %% default value 
	  inhibit = 0 :: timeout(),   %% ignore delay
	  delay   = 0 :: timeout(),   %% activation delay
	  rampup  = 0 :: timeout(),   %% ramp up time
	  sustain = 0 :: timeout(),   %% time to stay active 
	  rampdown = 0 :: timeout(),  %% ramp down time
	  deact   = 0 :: timeout(),         %% deactivation delay
	  wait    = 0 :: timeout(),         %% delay between re-activation
	  repeat  = 0 :: integer(),         %% pulse repeat count
	  feedback = false :: boolean(),    %% feedback frames as input 
	  min_value = 0 :: uint32(),        %% min value
	  max_value = 16#ffff :: uint32(),  %% max value
	  ramp_min = 20 :: uint32()         %% min time quanta in during ramp
	 }).

-record(state, {
	  config  = #opt {} :: #opt{},   %% config values
	  value   = 0 :: uint32(),       %% current value
	  counter = 0 :: uint32(),       %% repeat counter
	  tref    = undefined :: undefined | reference(),
	  tramp   = undefined :: undefined | reference(),
	  deactivate = false :: boolean(), %% set while deactivate
	  inhibited  = false :: boolean(), %% disallow activation
	  env     = [],                  %% enironment for last event
	  actions = []
	 }).


-define(STATE(Name), io:format("state: ~w\n", [(Name)])).
-define(verbose(Fmt,As), io:format((Fmt)++"\n", (As))).

%%%===================================================================
%%% API
%%%===================================================================

validate_flags(Flags) ->
    case set_options(Flags, #opt {}) of
	{ok,_} -> ok;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Flags, Actions) ->
    FsmOptions = [],
    %% FsmOptions = [{debug,[trace]}],
    gen_fsm:start_link(?MODULE, {Flags,Actions}, FsmOptions).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init({Flags,Actions}) ->
    case set_options(Flags, #opt {}) of
	{ok, Config} ->
	    Value = clamp(Config, Config#opt.default),
	    State = #state { config = Config, actions = Actions },
	    State1 = output_value(?HEX_ANALOG, Value, State),
	    {ok, state_off, State1};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
state_off(Event={digital,1,Src}, State) ->
    if State#state.inhibited ->
	    ?verbose("inhibited in state_off", []),
	        {next_state, state_off, State};
       true ->
	    do_activate(Event, State#state { env = [{source,Src}] })
    end;
state_off({analog,Val,Src}, State) ->
    %% fixme: only in state_on / state_sustain?
    Env = [{value,Val}, {source,Src}],
    State1 = action(?HEX_ANALOG, Val, State#state.actions, Env, State),
    {next_state, state_off, State1};
state_off(_Event, State) ->
    {next_state, state_off, State}.

state_on({digital,0,Src}, State) ->
    do_deactivate(init, State#state { env = [{source,Src}] });
state_on({analog,Val,Src}, S) ->
    Env = [{value,Val}, {source,Src}],
    S1 = action(?HEX_ANALOG, Val, S#state.actions, Env, S),
    {next_state, state_on, S1};
state_on(_Event, S) ->
    lager:debug("ignore event ~p", [_Event]),
    {next_state, state_on, S}.

%% activation delay, activation may be cancelled in this phase
%% by sending an digital 0 signal
state_delay(init, State) ->
    ?STATE(delay),
    Time = (State#state.config)#opt.delay,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, delay),
	    State1 = State#state { tref = TRef },
	    {next_state, state_delay, State1};
       true ->
	    state_rampup(init, State)
    end;
state_delay({timeout,TRef,delay}, State) when State#state.tref =:= TRef ->
    State1 = State#state { tref = undefined },
    state_rampup(init, State1);
state_delay({digital,0,Src}, State) ->
    lager:debug("activation cancelled from", [Src]),
    do_off(State);
state_delay(_Event, S) ->
    lager:debug("state_delay: ignore event ~p", [_Event]),
    {next_state, state_delay, S}.

%% Ramping up output over rampup time milliseconds
state_rampup(init, State=#state { config=Config }) ->
    ?STATE(rampup),
    Time = Config#opt.rampup,
    if Time > 0 ->
	    %% FIXME: start from A=State#state.value !!!
	    #opt { max_value=A1, min_value=A0, ramp_min=Tm} = Config,
	    A = State#state.value,
	    Ad = abs(A1 - A0),
	    Td0 = trunc(Time*(1/Ad)),
	    Td = max(Tm, Td0),
	    T = 1-((A-A0)/Ad),
	    Time1 = trunc(T*Time),
	    ?verbose("rampup: time=~w,time1=~w,Ad=~w,Td0=~w,Td=~w,A=~w", 
		      [Time, Time1, Ad, Td0, Td, A]),
	    TRef = gen_fsm:start_timer(Td, {delta,Td}),
	    TRamp = gen_fsm:start_timer(Time1, done),
	    State1 = State#state { tref=TRef,tramp=TRamp },
	    State2 = output_value(?HEX_ANALOG, 0, State1),
	    {next_state, state_rampup, State2};
       true ->
	    state_sustain(init, State)
    end;
state_rampup({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampup, State#state { tref=undefined} };
	Tr ->
	    Config = State#state.config,
	    #opt { max_value=A1, min_value=A0, rampup=Time } = Config,
	    T = (Time - Tr)/Time,
	    A = trunc((A1-A0)*T + A0),
	    ?verbose("rampup: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = output_value(?HEX_ANALOG, A, State1),
	    {next_state, state_rampup, State2}
    end;
state_rampup({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    #opt { max_value=A } = State#state.config,
    ?verbose("rampup: T=~w A=~w", [1.0, A]),
    State2 = output_value(?HEX_ANALOG, A, State1),
    state_sustain(init, State2);
state_rampup(Event={digital,0,Src}, State) ->
    lager:debug("rampup cancelled from", [Src]),
    gen_fsm:cancel_timer(State#state.tref),
    gen_fsm:cancel_timer(State#state.tramp),
    %% fixme pick up remain time and calculate ramp down from that level
    do_deactivate(Event, State#state { tref=undefined, tramp=undefined });
state_rampup(_Event, S) ->
    lager:debug("state_rampup: ignore event ~p", [_Event]),
    {next_state, state_rampup, S}.

%%
%%  active state temporary (sustain>0) or permanent(sustain=0)
%%

state_sustain(init, State) ->
    ?STATE(sustain),
    Env = [{value,1}|State#state.env],
    State1 = action(?HEX_DIGITAL, 1, State#state.actions, Env, State),
    Time = (State#state.config)#opt.sustain,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, sustain),
	    State2 = State1#state { tref = TRef },
	    {next_state, state_sustain, State2};
       true ->
	    {next_state, state_on, State1}
    end;
state_sustain({timeout,TRef,sustain}, State) when State#state.tref =:= TRef ->
    state_deact(init, State#state { tref=undefined });
state_sustain(Event={digital,0,Src}, State) ->
    lager:debug("sustain cancelled from", [Src]),
    gen_fsm:cancel_timer(State#state.tref),
    do_deactivate(Event, State#state { tref = undefined });
state_sustain(_Event, S) ->
    lager:debug("state_sustain: ignore event ~p", [_Event]),
    {next_state, state_sustain, S}.
    
%% deactivation delay
state_deact(init, State) ->
    ?STATE(deact),
    Time = (State#state.config)#opt.deact,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, deact),
	    State1 = State#state { tref = TRef },
	    {next_state, state_deact, State1};
       true ->
	    state_rampdown(init, State)
    end;
state_deact({timeout,TRef,deact}, State) when State#state.tref =:= TRef ->
    state_rampdown(init, State#state { tref=undefined });
state_deact(_Event={digital,1,Src}, State) ->
    if State#state.inhibited ->
	    ?verbose("inhibited in state_deact", []),
	        {next_state, state_deact, State};
       true ->
	    lager:debug("deact cancelled from", [Src]),
	    _Remain = gen_fsm:cancel_timer(State#state.tref),
	    %% calculate remain sustain? or think if this as reactivation?
	    state_sustain(init, State#state { tref = undefined, 
					      deactivate=false })
    end;
state_deact(_Event, S) ->
    lager:debug("state_deact: ignore event ~p", [_Event]),
    {next_state, state_deact, S}.

%% Ramping down output over rampup time milliseconds
state_rampdown(init, State=#state { config=Config }) ->
    ?STATE(rampdown),
    Time = Config#opt.rampdown,
    if Time > 0 ->
	    #opt { max_value=A1, min_value=A0, ramp_min=Tm} = Config,
	    A = State#state.value,
	    Ad = abs(A1 - A0),
	    Td0 = trunc(Time*(1/Ad)),
	    Td = max(Tm, Td0),
	    T = 1-((A1-A)/Ad),
	    Time1 = trunc(T*Time),
	    ?verbose("rampdown: time=~w,time1=~w,Ad=~w,Td0=~w,Td=~w,A=~w",
		     [Time,Time1,Ad,Td,Td0,A]),
	    TRef = gen_fsm:start_timer(Td, {delta,Td}),
	    TRamp = gen_fsm:start_timer(Time1, done),
	    State1 = State#state { tref=TRef,tramp=TRamp },
	    State2 = output_value(?HEX_ANALOG, A, State1),
	    {next_state, state_rampdown, State2};
       true ->
	    state_wait(init, State)
    end;
state_rampdown({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampdown, State#state {tref=undefined} };
	Tr ->
	    Config = State#state.config,
	    #opt { max_value=A1, min_value=A0, rampdown=Time } = Config,
	    T = (Time-Tr)/Time,
	    A = trunc((A0-A1)*T + A1),
	    ?verbose("rampdown: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = output_value(?HEX_ANALOG, A, State1),
	    {next_state, state_rampdown, State2}
    end;
state_rampdown({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    #opt { min_value=A } = State#state.config,
    ?verbose("rampdown: T=~w A=~w", [1.0, A]),
    State2 = output_value(?HEX_ANALOG, A, State1),
    state_wait(init, State2);
state_rampdown(_Event={digital,1,Src}, State) ->
    if State#state.inhibited ->
	    ?verbose("inhibited in state_rampdown", []),
	    {next_state, state_rampdown, State};
       true ->
	    lager:debug("rampdown cancelled from", [Src]),
	    _Remain = gen_fsm:cancel_timer(State#state.tref),
	    state_rampup(init, State#state { tref = undefined,
					     deactivate=false })
    end;
state_rampdown(_Event={digital,0,Src}, State) ->
    lager:debug("deactivate during rampdown from", [Src]),
    %% mark for deactivation - when ramp is done
    {next_state, state_rampdown, State#state { deactivate=true }};
state_rampdown(_Event, S) ->
    lager:debug("state_rampdown: ignore event ~p", [_Event]),
    {next_state, state_rampdown, S}.

state_wait(init, State) ->
    ?STATE(wait),
    Env = [{value,0}, State#state.env],
    State1 = action(?HEX_DIGITAL, 0, State#state.actions, Env, State),
    WaitTime = (State1#state.config)#opt.wait,
    if State1#state.deactivate =:= true;
       State1#state.counter =:= 0 ->
	    do_off(State1);
       true ->
	    %% context switch event if WaitTime = 0!
	    TRef = gen_fsm:start_timer(WaitTime, wait),
	    State2 = State1#state { tref = TRef },
	    {next_state, state_wait, State2}
    end;
state_wait({timeout,TRef,wait}, State) when State#state.tref =:= TRef ->
    Repeat = State#state.counter,
    if  Repeat =:= 0 -> %% should not happend?
	    do_off(State#state { tref=undefined} );
	Repeat =:= -1 -> %% interval - pulse forever
	    do_reactivate(init, State#state { tref=undefined });
	Repeat > 0 ->  %% pulse counter
	    do_reactivate(init, State#state { tref=undefined,
					      counter = Repeat-1 })
    end;
state_wait(_Event={digital,0,Src}, State) ->
    lager:debug("deactivate during wait from", [Src]),
    do_off(State);
state_wait(_Event, State) ->
    lager:debug("state_wait: ignore event ~p", [_Event]),
    {next_state, state_wait, State}.


do_activate(Event, State=#state{config=Config}) ->
    ?verbose("DO_ACTIVATE",[]),
    transmit_active(1, State),
    Inhibited = if Config#opt.inhibit > 0 -> 
			%% handle in handle_info instead of state since,
			%% this cover all states, there should be an
			%% gen_fsm:start_timer_all(...)
			erlang:start_timer(Config#opt.inhibit, self(), 
					   inhibit_done),
			?verbose("inhibit started for: ~w ms",
				 [Config#opt.inhibit]),
			true;
		   true -> false
		end,
    State1 = State#state { counter = Config#opt.repeat, 
			   inhibited = Inhibited },
    do_reactivate(Event, State1).

do_reactivate(_Event, State) ->
    ?verbose("DO_REACTIVATE",[]),
    %% state_delay(init, State).
    state_rampup(init, State).

do_deactivate(_Event, State) ->
    ?verbose("DO_DEACTIVATE",[]),
    transmit_active(0, State),
    state_deact(init, State#state { deactivate=true }).

do_off(State) ->
    ?verbose("DO_OFF",[]),
    _Remained = if is_reference(State#state.tref) ->
			gen_fsm:cancel_timer(State#state.tref);
		   true -> 0
		end,
    if State#state.deactivate ->
	    ok; %% already sent
       true ->
	    transmit_active(0, State)
    end,
    {next_state, state_off, State#state { tref=undefined, deactivate=false}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({timeout,_Ref,inhibit_done}, StateName, State) ->
    ?verbose("inhibitation stopped",[]),
    {next_state, StateName, State#state { inhibited=false}};

handle_info({Channel,Value={encoder,Delta,_Src}}, StateName, State) ->
    %% only when state_on?
    try get_option(Channel, State#state.config) of
	Value ->
	    %% wrap/clamp values here!
	    try set_option(Channel,Value+Delta,State#state.config) of
		Config -> 
		    {next_state, StateName, State#state { config=Config }}
	    catch
		error:Reason ->
		    lager:error("dynamic channel setting ~s, encoder ~w crash: ~p",
				[Channel,Delta,Reason]),
		    {next_state, StateName, State}
	    end
    catch
	error:Reason ->
		    lager:error("dynamic channel getting ~s crash: ~p",
				[Channel,Reason]),
	    {next_state, StateName, State}
    end;

handle_info({value,{analog,_V,_Src}}, StateName, State) ->
    %% Fixme; check valid states where analog values may be written
    {next_state, StateName, State};

handle_info({Channel,{analog,V,_Src}}, StateName, State) ->
    %% only when state_on?
    try set_option(Channel,V,State#state.config) of
	Config -> 
	    {next_state, StateName, State#state { config=Config }}
    catch
	error:Reason ->
	    lager:error("dynamic channel setting ~s, analog ~w crash: ~p",
			[Channel,V,Reason]),
	    {next_state, StateName, State}
    end;

handle_info({_Channel,{digital,_Val,_Src}}, StateName, State) ->
    %% no booean channels yet!
    {next_state, StateName, State};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%
%% Set output options
%%
set_options([{Option,Value} | Options], Opt) ->
    try set_option(Option, Value, Opt) of
	Opt1 -> set_options(Options, Opt1)
    catch
	error:_ ->
	    {error, {badarg, {Option, Value}}}
    end;
set_options([Option | Options], Opt) ->
    try set_option(Option, true, Opt) of
	Opt1 -> set_options(Options, Opt1)
    catch
	error:_ ->
	    {error, {badarg,Option}}
    end;
set_options([], Opt) ->
    {ok, Opt}.

set_option(K, V, Opt) ->
    case K of
	nodeid when ?is_uint32(V)   ->  Opt#opt { nodeid = V };
	chan when ?is_uint8(V)      ->  Opt#opt { chan = V };
	default when ?is_uint32(V)  ->  Opt#opt { default = V };
	inhibit when ?is_timeout(V) -> Opt#opt { inhibit = V };
	delay   when ?is_timeout(V) -> Opt#opt { delay = V };
	rampup  when ?is_timeout(V) -> Opt#opt { rampup = V };
	rampdown when ?is_timeout(V) -> Opt#opt { rampdown = V };
	sustain when ?is_timeout(V) -> Opt#opt { sustain = V };
	deact when ?is_timeout(V) -> Opt#opt { deact = V };
	wait when  ?is_timeout(V) -> Opt#opt { wait = V };
	repeat when is_integer(V), V >= -1 -> Opt#opt { repeat = V };
	feedback when is_boolean(V) -> Opt#opt { feedback = V };
	min_value when ?is_uint32(V) -> Opt#opt { min_value = V };
	max_value when ?is_uint32(V) -> Opt#opt { max_value = V };
	ramp_min when ?is_uint32(V) -> Opt#opt { ramp_min = max(20, V) }
    end.

get_option(K, Opt) ->
    case K of
	nodeid ->  Opt#opt.nodeid;
	chan   ->  Opt#opt.chan;
	default ->  Opt#opt.default;
	inhibit -> Opt#opt.inhibit;
	delay   -> Opt#opt.delay;
	rampup  -> Opt#opt.rampup;
	rampdown -> Opt#opt.rampdown;
	sustain   -> Opt#opt.sustain;
	deact     -> Opt#opt.deact;
	wait      -> Opt#opt.wait;
	repeat    -> Opt#opt.repeat;
	feedback  -> Opt#opt.feedback;
	min_value -> Opt#opt.min_value;
	max_value  -> Opt#opt.max_value;
	ramp_min   -> Opt#opt.ramp_min
    end.

clamp(_Config, undefined) -> undefined;
clamp(#opt { max_value=A1, min_value=A0}, Value) ->
    if A1 >= A0 -> min(A1, max(A0, Value));
       true -> min(A0, max(A1, Value))
    end.
    

make_self(NodeID) ->
    16#20000000 bor (2#0011 bsl 25) bor (NodeID band 16#1ffffff).

%% output activity on/off
transmit_active(Active, State) ->
    Opt = State#state.config,
    Signal = #hex_signal { id=make_self(Opt#opt.nodeid),
			   chan=Opt#opt.chan,
			   type=?HEX_OUTPUT_ACTIVE,
			   value=Active,
			   source={output,Opt#opt.chan}},
    Env = [],
    hex_server:transmit(Signal, Env).

%% generate an action value and store value
output_value(_Type, Value, State) ->
    %% call action matching !
    State#state { value = Value }.

%% output "virtual" feedback
feedback(Type,Value, State) ->
    Opt = State#state.config,
    if Opt#opt.feedback ->
	    Signal = #hex_signal { id=make_self(Opt#opt.nodeid),
				   chan=Opt#opt.chan,
				   type=Type,
				   value=Value,
				   source={output,Opt#opt.chan}},
	    Env = [],
	    hex_server:event(Signal, Env);
       true ->
	    ok
    end.


action(Type, Value, Action, Env, S) ->
    %% fixme: probably better to convert to analog, then
    %% have feedback input to remap to digital again.
    feedback(Type, Value, S),
    if is_list(Action) ->
	    action_list(Value, Action, Env, S);
       true ->
	    action_list(Value, [{Value,Action}], Env, S)
    end.

action_list(Value, [{Match,Action} | Actions], Env, S) ->
    case hex_server:match_value(Match, Value) of
	true ->
	    execute(Action, Env),
	    action_list(Value, Actions, Env, S);
	false ->
	    action_list(Value, Actions, Env, S)
    end;
action_list(_Value, [], _Env, S) ->
    S.

execute({App, AppFlags}, Env) ->
    try App:output(AppFlags, Env) of
	Result ->
	    lager:debug("execute ~p = ~p", [{App,AppFlags}, Result]),
	    Result
    catch
	error:Reason ->
	    lager:error("execute ~p = ~p", [{App,AppFlags}, {error,Reason}]),
	    {error,Reason}
    end.
