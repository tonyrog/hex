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
-export([setopts/2]).
-export([getopts/2]).
-export([event_spec/0]).

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

-record(target, {
	  name :: atom(),                %% name of target
	  type = clamp :: clamp | wrap,  %% style of value
	  in_min  = 0        :: uint32(),
	  in_max  = 16#fffff :: uint32(),
	  out_min = 0        :: uint32(),
	  out_max = 16#fffff :: uint32(),
	  %% calculated
	  pos :: integer(),      %% #opt.name, #opt.other => dict
	  delta :: float()       %% (out_max-out_min)/(in_max-in_min)
	}).
%%
%% note!  a short pulse must be at least 1 ms long, since 
%% sustain = 0 means that output goes into state_on.
%%

-record(opt, {
	  value   = 0 :: uint32(),         %% value
	  inhibit = 0 :: timeout(),        %% ignore delay
	  delay   = 0 :: timeout(),        %% activation delay
	  rampup  = 0 :: timeout(),        %% ramp up time
	  sustain = 0 :: timeout(),        %% time to stay active 
	  rampdown = 0 :: timeout(),       %% ramp down time
	  deact   = 0 :: timeout(),        %% deactivation delay
	  wait    = 0 :: timeout(),        %% delay between re-activation
	  repeat  = 0 :: integer(),        %% pulse repeat count
	  feedback = 0 :: uint1(),         %% feedback frames as input
	  transmit = 0 :: uint1(),         %% transmit frames 
	  other = dict:new() :: dict()     %% others value name => value
	 }).

-record(state, {
	  nodeid  = 0 :: uint32(),         %% id of hex node
	  chan    = 0 :: uint8(),          %% output number 1..254
	  ramp_min = 20 :: uint32(),       %% min time quanta in during ramp
	  targets :: dict(),               %% dictionary of name -> #target {}
	  in_config = #opt {} :: #opt{},   %% config values
	  out_config :: #opt{},            %% mapped config values
	  counter = 0 :: uint32(),         %% repeat counter
	  tref    = undefined :: undefined | reference(),
	  tramp   = undefined :: undefined | reference(),
	  deactivate = false :: boolean(), %% set while deactivate
	  inhibited  = false :: boolean(), %% disallow activation
	  env     = [],                    %% enironment for last event
	  actions = []
	 }).

-define(STATE(Name), io:format("state: ~w\n", [(Name)])).
-define(verbose(Fmt,As), io:format((Fmt)++"\n", (As))).

%%%===================================================================
%%% API
%%%===================================================================

setopts(Pid, Flags) ->
    gen_fsm:sync_send_all_state_event(Pid, {setopts,Flags}).

getopts(Pid, Flags) ->
    gen_fsm:sync_send_all_state_event(Pid, {getopts,Flags}).

validate_flags(Flags) ->
    hex:validate_flags(Flags, event_spec()).
%%    case set_options(Flags, #opt {}, dict:new(), #state{}) of
%%	{ok,_,_,_} -> ok;
%%	Error -> Error
%%    end.

event_spec() ->
    [
     {leaf, nodeid, [{type, uint32, []},
		     {default, 0, []}]},
     {leaf, chan, [{type, uint8, []},
		   {default, 0, []}]},
     {leaf, ramp_min, [{type, uint32, []},
		       {default, 20, []},
		       {description, "The min step in ms for each output "
			"change in ramp -up or -down.", []}
		      ]},
     {leaf, min_value, [{type,uint32, []},
			{default, 0, []},
			{description, "same as target.value.out_min", []}
		       ]},
     {leaf, max_value, [{type,uint32, []},
			{default, 16#ffff, []},
			{description, "same as target.value.out_max", []}
			]},
     {leaf, value, [{type,uint32, []},
		    {default, 0, []},
		    %% {config, false, []},
		    {description, "The input value", []}
		   ]},
     {leaf, inhibit, [{type,uint32, []},
		      {default, 0, []},
		      {description, "Inhibit re-activation for ms",[]}
		     ]},
     {leaf, delay, [{type,uint32,[]},
		    {default,0,[]},
		    {description, "Delay in ms before activation of output."
		     " If output is deactivated before this timeout "
		     "then the output is never activated.", []}
		   ]},
     {leaf, rampup, [{type,uint32,[]},
		     {default,0,[]},
		     {description, "The time in ms for the output signal "
		      " to reach it's max_value from its min_value.", []}
		     ]},
     {leaf, rampdown, [{type,uint32,[]},
		       {default,0,[]},
		       {description, "The time in ms for the output signal "
			" to reach it's min_value from its max_value.", []}
		      ]},
     {leaf, sustain, [{type,uint32,[]},
		      {default,0,[]},
		      {description, "The time in ms the output should stay at "
		       "its maximum value before rampdown starts. A value of 0 "
		       "means forever or until a deactivation signal arrives.",
		       []}
		      ]},
     {leaf, deact, [{type,uint32,[]},
		    {default,0,[]},
		    {description, "Deactivation delay in ms. If output is "
		     "reactivated again before this timeout the output is "
		     "never deactivated.", []}
		    ]},
     {leaf, wait, [{type,uint32,[]},		  
		   {default,0,[]},
		   {description, "Delay in ms before next pulse, used when "
		    "repeat is set to a non zero value.", []}
		  ]},
     {leaf, repeat, [{type, int32, 
		      [{range, [{-1,16#7fffffff}], []}]},
		     {default,0,[]},
		     {description, "Pulse repeat count. A value of -1 means "
		      "that the output repeat forever, or until a "
		      "desctivation signal arrives.", []}
		    ]},
     {leaf,feedback,[{type, boolean, []},
		     {default, false, []},
		     {description, "Feedback the output signal as an "
		      "input signal. Can be useful when implementing "
		      "time delays etc.", []}
		    ]},
     {leaf,transmit,[{type, boolean, []},
		     {default, false, []},
		     {description, "Transmit the signal using the "
		      "transmit configration. This is used to implement "
		      "signal distribution. A CAN transmit module allow "
		      "all nodes in the network to monitor the output "
		      "actions.", []}
		    ]},
     {list,target,
      [{description, 
	"Declare the name of the id in the action spec that "
	"will receive the scaled/mapped value. This is mandatory "
	"for all values destined for action.", []},
       {key, name, []},
       {leaf, name, [{type,string,[]},
		     {mandatory, true, []}]},
       {leaf, type, [{type,enumeration,
		      [{enum,clamp,[]},{enum,wrap,[]}]},
		     {default, clamp, []},
		     {description, "Input value mapping.", []}
		    ]},
       {leaf, in_min, [{type, uint32, []},
		       {default, 0, []}]},
       {leaf, in_max, [{type, uint32, []},
		       {default, 16#ffff, []}]},
       {leaf, out_min, [{type, uint32, []},
		       {default, 0, []}]},
       {leaf, out_max, [{type, uint32, []},
			{default, 16#ffff, []}]}
      ]}
    ].

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
    Value = #target { name=value, type=clamp, delta=1.0, pos=#opt.value },
    Targets = dict:from_list([{value,Value}]),
    case set_options(Flags, #opt { }, Targets, #state{}) of
	{ok, IConfig, Targets1, State1} ->
	    OConfig = map_config(IConfig, Targets1),
	    State2 = State1#state {
		       in_config  = IConfig,
		       out_config = OConfig,
		       targets = Targets1, 
		       actions = Actions },
	    {ok, state_off, State2};
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

%% FIXME: when do we process analog input 
state_off({analog,Val,Src}, State) ->
    Env = [{value,Val}, {source,Src}],
    %% Map me?
    State1 = action(?HEX_ANALOG, Val, State#state.actions, Env, State),
    {next_state, state_off, State1};
state_off(_Event, State) ->
    {next_state, state_off, State}.

state_on({digital,0,Src}, State) ->
    do_deactivate(init, State#state { env = [{source,Src}] });

%% FIXME: when do we process analog input 
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
    Time = (State#state.out_config)#opt.delay,
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
state_rampup(init, State) ->
    ?STATE(rampup),
    Time = (State#state.out_config)#opt.rampup,
    if Time > 0 ->
	    Tm = State#state.ramp_min,
	    #target { in_max=A1, in_min=A0 } = Trg = value_target(State),
	    A = (State#state.in_config)#opt.value,
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
	    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
	    {next_state, state_rampup, State2};
       true ->
	    #target { in_max=A } = Trg = value_target(State),
	    State1 = output_value(?HEX_ANALOG, A, Trg, State),
	    state_sustain(init, State1)
    end;
state_rampup({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampup, State#state { tref=undefined} };
	Tr ->
	    Time = (State#state.out_config)#opt.rampup,
	    #target { in_max=A1, in_min=A0 } = Trg = value_target(State),
	    T = (Time - Tr)/Time,
	    A = trunc((A1-A0)*T + A0),
	    ?verbose("rampup: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
	    {next_state, state_rampup, State2}
    end;
state_rampup({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    #target { in_max=A } = Trg = value_target(State),
    ?verbose("rampup: T=~w A=~w", [1.0, A]),
    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
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
    Time = (State#state.out_config)#opt.sustain,
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
    Time = (State#state.out_config)#opt.deact,
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
state_rampdown(init, State) ->
    ?STATE(rampdown),
    Time = (State#state.out_config)#opt.rampdown,
    if Time > 0 ->
	    Tm = State#state.ramp_min,
	    #target { in_max=A1, in_min=A0 } = Trg = value_target(State),
	    A = (State#state.in_config)#opt.value,
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
	    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
	    {next_state, state_rampdown, State2};
       true ->
	    #target { in_min=A } = Trg = value_target(State),
	    State1 = output_value(?HEX_ANALOG, A, Trg, State),
	    state_wait(init, State1)
    end;
state_rampdown({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampdown, State#state {tref=undefined} };
	Tr ->
	    Time = (State#state.out_config)#opt.rampdown,
	    #target { in_max=A1, in_min=A0 } = Trg = value_target(State),
	    T = (Time-Tr)/Time,
	    A = trunc((A0-A1)*T + A1),
	    ?verbose("rampdown: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
	    {next_state, state_rampdown, State2}
    end;
state_rampdown({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    #target { in_min=A } = Trg = value_target(State),
    ?verbose("rampdown: T=~w A=~w", [1.0, A]),
    State2 = output_value(?HEX_ANALOG, A, Trg, State1),
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
    WaitTime = (State1#state.out_config)#opt.wait,
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

do_activate(Event, State) ->
    ?verbose("DO_ACTIVATE",[]),
    transmit_active(1, State),
    OConfig = State#state.out_config,
    Inhibited = if OConfig#opt.inhibit > 0 -> 
			%% handle in handle_info instead of state since,
			%% this cover all states, there should be an
			%% gen_fsm:start_timer_all(...)
			erlang:start_timer(OConfig#opt.inhibit, self(), 
					   inhibit_done),
			?verbose("inhibit started for: ~w ms",
				 [OConfig#opt.inhibit]),
			true;
		   true -> false
		end,
    State1 = State#state { counter = OConfig#opt.repeat, 
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

handle_sync_event({getopts,Flags}, _From, StateName, State) ->
    try get_options(Flags, State#state.in_config, State, []) of
	Result ->
	    {reply, {ok,Result}, StateName, State}
    catch
	error:Reason ->
	    {reply, {error,Reason}, StateName, State}
    end;

handle_sync_event({setopts,Opts}, _From, StateName, State) ->
    case set_options(Opts, State#state.in_config,State#state.targets,State) of
	{ok, IConfig, Targets, State1} ->
	    %% FIXME: map to output configs here!
	    {reply, ok, StateName, 
	     State1#state { in_config=IConfig, targets=Targets }};
	Error ->
	    {reply, Error, StateName, State}
    end;

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

%% only when state_on?
handle_info(_Info={Name,{encoder,Delta,_Src}}, StateName, State) ->
    case dict:find(Name, State#state.targets) of
	error ->
	    lager:error("target ~s not found: event ~p", 
			[Name, _Info]),
	    {next_state, StateName, State};
	{ok,Target} ->
	    IConfig = State#state.in_config,
	    X = case Target#target.pos of
		    #opt.other ->
			dict:fetch(Target#target.name, IConfig#opt.other);
		    Pos ->
			element(Pos, IConfig)
		end,
	    {Value,State1} = set_value(Target, X, Delta, State),
	    Env = [{Name,Value}],
	    State2 = action(?HEX_ANALOG,Value,State#state.actions,Env,State1),
	    {next_state, StateName, State2}
    end;
%% only when state_on?
handle_info(_Info={Name,{Type,X,_Src}}, StateName, State) when
      Type =:= analog;
      Type =:= ?HEX_ANALOG ->
    case dict:find(Name, State#state.targets) of
	error ->
	    lager:error("target ~s not found: event ~p", 
			[Name, _Info]),
	    {next_state, StateName, State};
	{ok,Target} ->
	    {Value,State1} = set_value(Target, X, 0, State),
	    Env = [{Name,Value}],
	    State2 = action(?HEX_ANALOG,Value,State#state.actions,
			    Env,State1),
	    {next_state, StateName, State2}
    end;
handle_info(_Info={Name,{Type,X,_Src}}, StateName, State) 
  when Type =:= digital; 
       Type =:= ?HEX_DIGITAL;
       Type =:= ?HEX_OUTPUT_ACTIVE ->
    case dict:find(Name, State#state.targets) of
	error ->
	    lager:error("target ~s not found: event ~p", 
			[Name, _Info]),
	    {next_state, StateName, State};
	{ok,Target} ->
	    {Value,State1} = set_value(Target, X, 0, State),
	    Env = [{Name,Value}],
	    State2 = action(?HEX_DIGITAL,Value,State#state.actions,Env,State1),
	    {next_state, StateName, State2}
    end;
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

value_target(State) ->
    %% maybe cache in State?
    dict:fetch(value,State#state.targets).

%%
%% Set output options
%%
set_options([Opt|Options], Config, Targets, State) ->
    case Opt of
	{nodeid,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { nodeid=Value },
		    set_options(Options, Config, Targets, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;
	{chan,Value} ->
	    if ?is_uint8(Value) ->
		    State1 = State#state { chan=Value },
		    set_options(Options, Config, Targets, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;
	{ramp_min,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { ramp_min=min(20,Value) },
		    set_options(Options, Config, Targets, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;
	{min_value,Value} when ?is_uint32(Value) ->
	    {ok,Target} = dict:find(value, Targets),
	    Target1 = Target#target { out_min = Value },
	    Targets1 = dict:store(value, Target1, Targets),
	    set_options(Options, Config, Targets1, State);
	{max_value,Value} when ?is_uint32(Value) ->
	    {ok,Target} = dict:find(value, Targets),
	    Target1 = Target#target { out_max = Value },
	    Targets1 = dict:store(value, Target1, Targets),
	    set_options(Options, Config, Targets1, State);
	{target,Value} ->
	    try set_target(Value, Targets) of
		Targets1 -> set_options(Options, Config, Targets1, State)
	    catch
		error:_ ->
		    {error, {badarg, Opt}}
	    end;
	{Option,Value} ->
	    try set_option(Option, Value, Config) of
		Config1 -> set_options(Options,Config1,Targets,State)
	    catch
		error:_ ->
		    {error, {badarg, Opt}}
	    end;
	_ when is_atom(Opt) ->
	    try set_option(Opt, true, Config) of
		Config1 -> set_options(Options,Config1,Targets,State)
	    catch
		error:_ ->
		    {error, {badarg,Opt}}
	    end;
	_ ->
	    {error, {badarg,Opt}}
    end;
set_options([], Config, Targets, State) ->
    {ok, Config, Targets, State}.

set_option(K, V, Config) ->
    case K of
	value when ?is_uint32(V)  ->  Config#opt { value = V };
	inhibit when ?is_timeout(V) -> Config#opt  { inhibit = V };
	delay   when ?is_timeout(V) -> Config#opt  { delay = V };
	rampup  when ?is_timeout(V) -> Config#opt  { rampup = V };
	rampdown when ?is_timeout(V) -> Config#opt { rampdown = V };
	sustain when ?is_timeout(V) -> Config#opt  { sustain = V };
	deact when ?is_timeout(V) -> Config#opt    { deact = V };
	wait when  ?is_timeout(V) -> Config#opt    { wait = V };
	repeat when is_integer(V), V >= -1 -> Config#opt { repeat = V };
	feedback when V =:= true -> Config#opt  { feedback = 1 };
	feedback when V =:= false -> Config#opt  { feedback = 0 };
	feedback when ?is_uint1(V) -> Config#opt  { feedback = V };
	transmit when V =:= true -> Config#opt  { transmit = 1 };
	transmit when V =:= false -> Config#opt  { transmit = 0 };
	transmit when ?is_uint1(V) -> Config#opt  { transmit = V };
	_ when ?is_uint32(V) ->
	    Config#opt { other = dict:store(K,V,Config#opt.other) }
    end.

%% set or update a target
set_target(Options, Dict) ->
    {name,Name} = proplists:lookup(name, Options),
    case dict:find(Name, Dict) of
	error ->
	    set_target(Options, #target {}, Dict);
	{ok,Target} ->
	    set_target(Options, Target, Dict)
    end.

set_target([{K,V}|Opts], Target, Dict) ->
    case K of
	name when is_atom(V) -> 
	    set_target(Opts, Target#target { name=V }, Dict);
	type when V =:= clamp; V =:= wrap ->
	    set_target(Opts, Target#target { type=V }, Dict);
	in_min when ?is_uint32(V) -> 
	    set_target(Opts, Target#target { in_min = V }, Dict);
	in_max when ?is_uint32(V) -> 
	    set_target(Opts, Target#target { in_max = V }, Dict);
	out_min when ?is_uint32(V) -> 
	    set_target(Opts, Target#target { out_min = V }, Dict);
	out_max when ?is_uint32(V) ->
	    set_target(Opts, Target#target { out_max = V }, Dict)
    end;
set_target([], Target, Dict) ->
    #target { in_min = X0, in_max = X1, out_min = Y0, out_max = Y1 } = Target,
    Delta = (Y1-Y0) / (X1 - X0),
    Name = Target#target.name,
    Target1 = Target#target { delta = Delta, pos = target_pos(Name) },
    dict:store(Target1#target.name, Target1, Dict).

set_value(Target, X, Delta, State) ->
    Xi = constrain_in_value(X + Delta, Target),
    Xo = map_value(Xi, Target),
    ?verbose("set_value target:~s delta:~w in:~w, constrained:~w, mapped:~w",
	     [Target#target.name,Delta,X,Xi,Xo]),
    Pos = Target#target.pos,
    IConfig = State#state.in_config,
    OConfig = State#state.out_config,
    State1 =
	if Pos =:= #opt.other ->
		IOther = dict:store(Target#target.name, Xi, IConfig#opt.other),
		OOther = dict:store(Target#target.name, Xo, OConfig#opt.other),
		State#state { in_config = IConfig#opt { other = IOther },
			      out_config = OConfig#opt { other = OOther }};
	   true ->
		State#state {  in_config  = setelement(Pos, IConfig, Xi),
			       out_config = setelement(Pos, OConfig, Xo) }
	end,
    {Xo,State1}.


map_value(X, #target { in_min=X0, out_min=Y0, delta=D }) ->
    trunc((X - X0) * D + Y0).

%% map all input configs through targets dictionary
map_config(ConfigIn, Targets) ->
    dict:fold(
      fun (Name,Target=#target{pos=#opt.other},ConfigOut) ->
	      X = case dict:find(Name, ConfigIn#opt.other) of
		      error -> 0;
		      {ok,X0} -> X0
		  end,
	      Xi = constrain_in_value(X, Target),
	      Xo = map_value(Xi, Target),
	      ?verbose("target ~s in:~w, constrained:~w, mapped:~w",
		       [Name,X,Xi,Xo]),
	      Other = dict:store(Name, Xo, ConfigOut#opt.other),
	      ConfigOut#opt { other = Other };
	  (Name,Target=#target{pos=Pos},ConfigOut) ->
	      X = element(Pos,ConfigIn),
	      Xi = constrain_in_value(X, Target),
	      Xo = map_value(Xi, Target),
	      ?verbose("target ~s in:~w, constrained:~w, mapped:~w",
		       [Name,X,Xi,Xo]),
	      setelement(Pos, ConfigOut, Xo)
      end, ConfigIn, Targets).

constrain_in_value(X, Target) when Target#target.type =:= wrap ->
    wrap_in_value(X, Target);
constrain_in_value(X, Target) when Target#target.type =:= clamp ->
    clamp_in_value(X, Target).

clamp_in_value(X, #target { in_min=X0, in_max=X1 }) ->
    if X1 >= X0 -> min(X1, max(X0, X));
       true -> min(X0, max(X1, X))
    end.

constrain_out_value(X, Target) when Target#target.type =:= wrap ->
    wrap_out_value(X, Target);
constrain_out_value(X, Target) when Target#target.type =:= clamp ->
    clamp_out_value(X, Target).

clamp_out_value(Y, #target { out_min=Y0, out_max=Y1 }) ->
    if Y1 >= Y0 -> min(Y1, max(Y0, Y));
       true -> min(Y0, max(Y1, Y))
    end.

wrap_in_value(X, #target { in_min=X0, in_max=X1 }) ->
    if X1 >= X0 ->
	    mod((X - X0),(X1 - X0 + 1)) + X0;
       X1 < X0 ->
	    mod((X - X1),(X0 - X1 + 1)) + X1
    end.

wrap_out_value(Y, #target { out_min=Y0, out_max=Y1 }) ->
    if Y1 >= Y0 ->
	    mod((Y - Y0),(Y1 - Y0 + 1)) + Y0;
       Y1 < Y0 ->
	    mod((Y - Y1),(Y0 - Y1 + 1)) + Y1
    end.

mod(A,N) ->
    A1 = A rem N,
    if A1 < 0 -> 
	    A1 + N;
       true ->
	    A1
    end.

target_pos(K) ->
    case K of
	value     -> #opt.value;
	inhibit   -> #opt.inhibit;
	delay     -> #opt.delay;
	rampup    -> #opt.rampup;
	rampdown  -> #opt.rampdown;
	sustain   -> #opt.sustain;
	deact     -> #opt.deact;
	wait      -> #opt.wait;
	repeat    -> #opt.repeat;
	feedback  -> #opt.feedback;
	_ ->         #opt.other  %% (put in dict)
    end.

get_options([nodeid|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{nodeid,State#state.nodeid}|Acc]);
get_options([chan|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{chan,State#state.chan}|Acc]);
get_options([ramp_min|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{ramp_min,State#state.ramp_min}|Acc]);
get_options([K|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{K,get_option(K,Config)}|Acc]);
get_options([], _Config, _State, Acc) ->
    lists:reverse(Acc).

get_option(K, Config) ->
    case K of
	value     -> Config#opt.value;
	inhibit   -> Config#opt.inhibit;
	delay     -> Config#opt.delay;
	rampup    -> Config#opt.rampup;
	rampdown  -> Config#opt.rampdown;
	sustain   -> Config#opt.sustain;
	deact     -> Config#opt.deact;
	wait      -> Config#opt.wait;
	repeat    -> Config#opt.repeat;
	feedback  -> Config#opt.feedback;
	_ -> dict:fetch(K, Config#opt.other)
    end.

make_self(NodeID) ->
    if NodeID band 16#02000000 =/= 0 ->
	    16#20000000 bor (2#0011 bsl 25) bor (NodeID band 16#1ffffff);
       true ->
	    (2#0011 bsl 9) bor (NodeID band 16#7f)
    end.

%% output activity on/off
transmit_active(Active, State) ->
    Signal = #hex_signal { id=make_self(State#state.nodeid),
			   chan=State#state.chan,
			   type=?HEX_OUTPUT_ACTIVE,
			   value=Active,
			   source={output,State#state.chan}},
    Env = [],
    feedback_signal(Signal, Env, State),
    hex_server:transmit(Signal, Env).

feedback_signal(Signal, Env, State) when 
      (State#state.out_config)#opt.feedback =:= 1 ->
    hex_server:event(Signal, Env);
feedback_signal(_Signal, _Env, _State) ->
    ok.

transmit_signal(Signal, Env, State) when 
      (State#state.out_config)#opt.transmit =:= 1 ->
    hex_server:transmit(Signal, Env);
transmit_signal(_Signal, _Env, _State) ->
    ok.


%% generate an action value and store value
output_value(_Type, Vin, Target, State) ->
    Vout = map_value(Vin, Target),
    %% call action matching !
    IConfig = State#state.in_config,
    OConfig = State#state.out_config,
    State#state {  in_config = IConfig#opt { value = Vin },
		   out_config = OConfig#opt { value = Vout }}.

%% output "virtual" feedback
feedback(Type,Value, State) ->
    Signal = #hex_signal { id=make_self(State#state.nodeid),
			   chan=State#state.chan,
			   type=Type,
			   value=Value,
			   source={output,State#state.chan}},
    Env = [],
    feedback_signal(Signal, Env, State),
    transmit_signal(Signal, Env, State).


action(Type, Value, Action, Env, S) ->
    lager:debug("action type=~.16B, value=~w, env=~w\n", [Type,Value,Env]),
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
