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
	  active  = 0 :: uint32(),         %% active output value
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
	  other = dict:new() :: dict:dict() %% others value name => value
	 }).

-type core_var() :: atom() | integer().

-record(state, {
	  nodeid  = 0 :: uint32(),         %% id of hex node
	  chan    = 0 :: uint8(),          %% output number 1..254
	  ramp_min = 20 :: uint32(),       %% min time quanta in during ramp
	  targets :: dict:dict(),          %% dictionary of name -> #target {}
	  in_config = #opt {} :: #opt{},   %% config values
	  out_config :: #opt{},            %% mapped config values
	  counter = 0 :: uint32(),         %% repeat counter
	  tref    = undefined :: undefined | reference(),
	  tramp   = undefined :: undefined | reference(),
	  digital = true,                  %% output signal type
	  deactivate = false :: boolean(), %% set while deactivate
	  inhibited  = false :: boolean(), %% disallow activation

	  enable = "value" :: string(),       %% enable expression
	  enable_var :: core_var(),             %% enable variable

	  disable = "not value" :: string(),  %% diable expression
	  disable_var  :: core_var(),          %% disable variable

	  active = "value" :: string(),       %% active expression
	  active_var :: core_var(),            %% active variable

	  inactive = "not value" :: string(), %% inactive expr
	  inactive_var :: core_var(),          %% inactive variable

	  output = "value" :: string(),        %% output expression
	  output_var :: core_var(),            %% output variable

	  enable_value = 0,                    %% last enable value
	  active_value = 0,                    %% last active value

	  core = hex_core:new() :: term(),

	  env     = [],                    %% enironment for last event
	  actions = []                     %% [{Cond,Value,{App,Flags}}]
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
    %% fixme: special treat 'active' and 'output_active'
    hex:validate_flags(Flags, event_spec()).

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
     {leaf, input, [{type,uint32, []},
		    {default, 0, []},
		    %% {config, false, []},
		    {description, "The input value", []}
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
		    {description, "The output value", []}
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
     {leaf,enable,[{type,string,[]},
		   {default,"value", []},
		   {description, "Boolean expression describing "
		    "when an output is enabled. That is, "
		    "the output is unlatched."}
		  ]},
     {leaf,disable,[{type,string,[]},
		    {default,"not enable", []},
		    {description, "Boolean expression describing "
		     "when an output is disabled, that is the output is "
		     "latched.", []}
		   ]},
     {leaf,active, [{type,string,[]},
		     {default,"value", []},
		     {description, "Boolean expression describing when an "
		      "activation signal will be sent.",[]}
		    ]},
     {leaf,inactive, [{type,string,[]},
		      {default,"not value", []},
		      {description, "Boolean expression describing when an "
		       "deactivation signal will be sent.",[]}
		     ]},
     {leaf,output, [{type,string,[]},
		    {default,"value", []},
		    {description, "Expression to calculte the output value.",
		    []}
		   ]},
     {leaf,digital, [{type,boolean,[]},
		     {default,true,[]},
		     {description, "if 'digital' is true the a digital "
		      "signal is sent in transmit or feedback otherwise "
		      "an analog signal is sent.",
		      []}
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
init({Flags,Actions0}) ->
    Value  = #target { name=value, type=clamp, delta=1.0, pos=#opt.value },
    Targets = dict:from_list([{value,Value}]),
    Actions = rewrite_actions(Actions0),
    State0 = #state { actions = Actions,
		      in_config = #opt {}, 
		      out_config = #opt {},
		      targets = Targets },
    %% fixme run initial eval!
    %% fixme: send input mapping here?
    case set_options(Flags, State0) of
	{ok, State1} ->
	    Core1 = hex_core:set_value(value, 0, State1#state.core),
	    Core2 = hex_core:eval(Core1),

	    A = case hex_core:value(State0#state.active_var, Core2) of
		    0 -> transmit_active(0, State1), 0;
		    _ -> transmit_active(1, State1), 1
		end,
	    transmit_active(A, State1),

	    case hex_core:value(State1#state.enable_var, Core2) of
		0 ->
		    {ok, state_off, State1#state { enable_value=0,
						   active_value=A,
						   core=Core2 }};
		V ->
		    {ok, state_on, State1#state { enable_value=V,
						  active_value=A,
						  core=Core2 }}
	    end;
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
state_off(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    {next_state, state_off, State1};
	{1,State1} ->
	    if State1#state.inhibited ->
		    ?verbose("inhibited in state_off", []),
		    {next_state, state_off, State1};
	       true ->
		    {_,{_,_,Src}} = Event,
		    do_activate(Event, State1#state { env = [{source,Src}] })
	    end
    end;
state_off(_Event, State) ->
    lager:debug("ignore event ~p", [_Event]),
    {next_state, state_off, State}.

state_on(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    do_deactivate(init, State1#state { env = [{source,Src}] });
	{1, State1} ->
	    {next_state, state_on, State1}
    end;
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
state_delay(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    lager:debug("activation cancelled from ~p", [Src]),
	    do_off(State1);
	{1,State1} ->
	    {next_state, state_delay, State1}
    end;
state_delay(_Event, State) ->
    lager:debug("state_delay: ignore event ~p", [_Event]),
    {next_state, state_delay, State}.

%% Ramping up output over rampup time milliseconds
state_rampup(init, State) ->
    ?STATE(rampup),
    Time = (State#state.out_config)#opt.rampup,
    if Time > 0 ->
	    Tm = State#state.ramp_min,
	    #target { in_max=A1, in_min=A0 } = value_target(State),
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
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampup, State2};
       true ->
	    State1 = do_analog_value(max,State),
	    state_sustain(init, State1)
    end;
state_rampup({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampup, State#state { tref=undefined} };
	Tr ->
	    Time = (State#state.out_config)#opt.rampup,
	    #target { in_max=A1, in_min=A0 } = value_target(State),
	    T = (Time - Tr)/Time,
	    A = trunc((A1-A0)*T + A0),
	    ?verbose("rampup: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampup, State2}
    end;
state_rampup({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    ?verbose("rampup: T=~w A=~w", [1.0, max]),
    State2 = do_analog_value(max,State1),
    state_sustain(init, State2);

state_rampup(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0,State1} ->
	    lager:debug("rampup cancelled from ~p", [Src]),
	    gen_fsm:cancel_timer(State1#state.tref),
	    gen_fsm:cancel_timer(State1#state.tramp),
	    do_deactivate(Event, State1#state { tref=undefined, 
						tramp=undefined });
	{1,State1} ->
	    {next_state, state_rampup, State1}
    end;
state_rampup(_Event, S) ->
    lager:debug("state_rampup: ignore event ~p", [_Event]),
    {next_state, state_rampup, S}.

%%
%%  active state temporary (sustain>0) or permanent(sustain=0)
%%

state_sustain(init, State) ->
    ?STATE(sustain),
    State1 = do_analog_value(max,State),
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

state_sustain(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0,State1} ->
	    lager:debug("sustain cancelled from ~p", [Src]),
	    gen_fsm:cancel_timer(State1#state.tref),
	    %% was Event
	    do_deactivate(init, State1#state { tref = undefined });
	{1,State1} ->
	    {next_state, state_sustain, State1}
    end;
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

state_deact(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    {next_state, state_deact, State1};
	{1, State1} ->
	    if State1#state.inhibited ->
		    ?verbose("inhibited in state_deact", []),
		    {next_state, state_deact, State1};
	       true ->
		    lager:debug("deact cancelled from ~p", [Src]),
		    _Remain = gen_fsm:cancel_timer(State#state.tref),
		    %% FIXME: calculate remain sustain? 
		    %% or think if this as reactivation?
		    state_sustain(init, State1#state { tref = undefined, 
						       deactivate=false })
	    end
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
	    #target { in_max=A1, in_min=A0 } = value_target(State),
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
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampdown, State2};
       true ->
	    State1 = do_analog_value(min,State),
	    state_wait(init, State1)
    end;
state_rampdown({timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampdown, State#state {tref=undefined} };
	Tr ->
	    Time = (State#state.out_config)#opt.rampdown,
	    #target { in_max=A1, in_min=A0 } = value_target(State),
	    T = (Time-Tr)/Time,
	    A = trunc((A0-A1)*T + A1),
	    ?verbose("rampdown: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampdown, State2}
    end;
state_rampdown({timeout,TRef,done}, State) when State#state.tramp =:= TRef ->
    if is_reference(State#state.tref) ->
	    gen_fsm:cancel_timer(State#state.tref);
       true ->
	    ok
    end,
    State1 = State#state { tramp=undefined, tref=undefined },
    ?verbose("rampdown: T=~w A=~w", [1.0, min]),
    State2 = do_analog_value(min,State1),
    state_wait(init, State2);
state_rampdown(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    lager:debug("deactivate during rampdown from ~p", [Src]),
	    %% mark for deactivation - when ramp is done
	    {next_state, state_rampdown, State1#state { deactivate=true }};
	{1, State1} ->
	    if State1#state.inhibited ->
		    ?verbose("inhibited in state_rampdown", []),
		    {next_state, state_rampdown, State1};
	       true ->
		    lager:debug("rampdown cancelled from ~p", [Src]),
		    _Remain = gen_fsm:cancel_timer(State1#state.tref),
		    state_rampup(init, State1#state { tref = undefined,
						      deactivate=false })
	    end
    end;
state_rampdown(_Event, S) ->
    lager:debug("state_rampdown: ignore event ~p", [_Event]),
    {next_state, state_rampdown, S}.

state_wait(init, State) ->
    ?STATE(wait),
    State1 = do_analog_value(min,State),
    WaitTime = (State1#state.out_config)#opt.wait,
    if State1#state.enable_value =:= 0, State1#state.counter =:= 0 ->
	    do_off(State1);
       true ->
	    %% context switch even if WaitTime = 0!
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

state_wait(Event={Name,{_Type,Value,Src}}, State) when 
      is_atom(Name), is_integer(Value) ->
    case do_input(Event, State) of
	{0, State1} ->
	    lager:debug("deactivate during wait from ~p", [Src]),
	    do_off(State1);
	{1, State1} ->
	    {next_state, state_wait, State1}
    end;
state_wait(_Event, State) ->
    lager:debug("state_wait: ignore event ~p", [_Event]),
    {next_state, state_wait, State}.

do_activate(Event, State) ->
    ?verbose("DO_ACTIVATE",[]),
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
    state_deact(init, State).

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
    case set_options(Opts, State) of
	{ok, State1} ->
	    {reply, ok, StateName, State1};
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
set_options([Opt|Options], State) ->
    case Opt of
	{nodeid,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { nodeid=Value },
		    set_options(Options, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{enable,Value} when is_list(Value) ->
	    try hex_core:parse(Value) of
		Expr ->
		    {Var,Core} = hex_core:add_expr(Expr,State#state.core),
		    State1 = State#state { enable = Value,
					   enable_var = Var,
					   core = Core },
		    set_options(Options, State1)
	    catch
		error:_Reason ->
		    {error, {badarg,Opt}}
	    end;

	{disable,Value} when is_list(Value) ->
	    try hex_core:parse(Value) of
		Expr ->
		    {Var,Core} = hex_core:add_expr(Expr,State#state.core),
		    State1 = State#state { disable = Value,
					   disable_var= Var,
					   core = Core },
		    set_options(Options, State1)
	    catch
		error:_Reason ->
		    {error, {badarg,Opt}}
	    end;

	{active,Value} when is_list(Value) ->
	    try hex_core:parse(Value) of
		Expr ->
		    {Var,Core} = hex_core:add_expr(Expr,State#state.core),
		    State1 = State#state { active = Value,
					   active_var = Var,
					   core = Core },
		    set_options(Options, State1)
	    catch
		error:_Reason ->
		    {error, {badarg,Opt}}
	    end;

	{inactive,Value} when is_list(Value) ->
	    try hex_core:parse(Value) of
		Expr ->
		    {Var,Core} = hex_core:add_expr(Expr,State#state.core),
		    State1 = State#state { inactive = Value,
					   inactive_var = Var,
					   core = Core },
		    set_options(Options, State1)
	    catch
		error:_Reason ->
		    {error, {badarg,Opt}}
	    end;

	{output,Value} when is_list(Value) ->
	    try hex_core:parse(Value) of
		Expr ->
		    {Var,Core} = hex_core:add_expr(Expr,State#state.core),
		    State1 = State#state { output = Value,
					   output_var = Var,
					   core = Core },
		    set_options(Options, State1)
	    catch
		error:_Reason ->
		    {error, {badarg,Opt}}
	    end;

	{digital,Value} when is_boolean(Value) ->
	    State1 = State#state { digital = Value },
	    set_options(Options, State1);

	{chan,Value} ->
	    if ?is_uint8(Value) ->
		    State1 = State#state { chan=Value },
		    set_options(Options, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{ramp_min,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { ramp_min=min(20,Value) },
		    set_options(Options, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{min_value,Value} when ?is_uint32(Value) ->
	    %% same as: setopts([{target,[{name,value},{out_min,Value}]}])
	    Targets = State#state.targets,
	    {ok,Target} = dict:find(value, Targets),
	    Target1 = Target#target { out_min = Value },
	    Targets1 = dict:store(value, Target1, Targets),
	    set_options(Options, State#state { targets=Targets1 });

	{max_value,Value} when ?is_uint32(Value) ->
	    %% same as: setopts([{target,[{name,value},{out_max,Value}]}])
	    Targets = State#state.targets,
	    {ok,Target} = dict:find(value, Targets),
	    Target1 = Target#target { out_max = Value },
	    Targets1 = dict:store(value, Target1, Targets),
	    set_options(Options, State#state { targets=Targets1 });

	{target,Value} ->
	    Targets = State#state.targets,
	    try set_target(Value, Targets) of
		Target ->
		    Targets1 = dict:store(Target#target.name,Target,Targets),
		    X = target_in_value(Target, State),
		    {_Out,State1} = set_value(Target, X, 0, State),
		    set_options(Options, State1#state { targets=Targets1 })
	    catch
		error:_ ->
		    {error, {badarg, Opt}}
	    end;

	{Name,Value} ->
	    try set_option(Name, Value, State#state.in_config) of
		IC ->
		    State1 = State#state { in_config=IC },
		    case dict:find(Name, State1#state.targets) of
			error ->
			    OC = set_option(Name,Value,State1#state.out_config),
			    set_options(Options, State1#state {out_config=OC});
			{ok,Target} ->
			    {_Out,State2}=set_value(Target,Value,0,State1),
			    set_options(Options, State2)
		    end
	    catch
		error:_ ->
		    {error, {badarg, Opt}}
	    end;
	_ ->
	    {error, {badarg,Opt}}
    end;
set_options([], State) ->
    {ok, State}.

set_option(K, V, Config) ->
    case K of
	value when ?is_uint32(V) ->  Config#opt { value = V };
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
	    set_target_(Options, #target {});
	{ok,Target} ->
	    set_target_(Options, Target)
    end.

set_target_([{K,V}|Opts], Target) ->
    case K of
	name when is_atom(V) ->
	    set_target_(Opts, Target#target { name=V });
	name when is_list(V) ->
	    set_target_(Opts, Target#target { name=list_to_atom(V) });
	type when V =:= clamp; V =:= wrap ->
	    set_target_(Opts, Target#target { type=V });
	in_min when ?is_uint32(V) -> 
	    set_target_(Opts, Target#target { in_min = V });
	in_max when ?is_uint32(V) -> 
	    set_target_(Opts, Target#target { in_max = V });
	out_min when ?is_uint32(V) -> 
	    set_target_(Opts, Target#target { out_min = V });
	out_max when ?is_uint32(V) ->
	    set_target_(Opts, Target#target { out_max = V })
    end;
set_target_([], Target) ->
    #target { in_min = X0, in_max = X1, out_min = Y0, out_max = Y1 } = Target,
    Delta = (Y1-Y0) / (X1 - X0),
    Name = Target#target.name,
    Target#target { delta = Delta, pos = target_pos(Name) }.


value_target(State) ->
    dict:fetch(value,State#state.targets).

target_in_value(Target, State) ->
    target_value(Target, State#state.in_config).

%% target_out_value(Target, State) ->
%%    target_value(Target, State#state.out_config).
    
target_value(Target, Config) ->
    case Target#target.pos of
	#opt.other ->
	    case dict:find(Target#target.name, Config#opt.other) of
		error -> 0;
		{ok,X} -> X
	    end;
	Pos ->
	    element(Pos, Config)
    end.


do_input({Name,{digital,Value,Src}}, State) ->
    do_input(?HEX_DIGITAL,Name, Value, 0, Src, State);
do_input({Name,{analog,Value,Src}}, State) ->
    do_input(?HEX_ANALOG,Name, Value, 0, Src, State);
do_input({Name,{encoder,Delta,Src}}, State) ->
    do_input(?HEX_ANALOG,Name, 0, Delta, Src, State);
do_input({Name,{?HEX_DIGITAL,Value,Src}}, State) ->
    do_input(?HEX_DIGITAL,Name, Value, 0, Src, State);
do_input({Name,{?HEX_ANALOG,Value,Src}}, State) ->
    do_input(?HEX_ANALOG,Name, Value, 0, Src, State);
do_input({Name,{?HEX_ENCODER,Delta,Src}}, State) ->
    do_input(?HEX_ANALOG,Name, 0, Delta, Src, State);
do_input({Name,{?HEX_OUTPUT_ACTIVE,Value,Src}}, State) ->
    do_input(?HEX_DIGITAL,Name, Value, 0, Src, State);
do_input(_Event, State) ->
    lager:debug("ignore event ~p", [_Event]),
    {State#state.enable_value, State}.

do_analog_value(min, State) ->
    #target { in_min=A } = value_target(State),
    do_analog_value(A, State);
do_analog_value(max, State) ->
    #target { in_max=A } = value_target(State),
    do_analog_value(A, State);
do_analog_value(A, State) when is_integer(A) -> %% allow number soon?
    %% FIXME: how to handle enable here?
    {_Enable,State1} = do_input(?HEX_ANALOG, value, A, 0, internal, State),
    State1.

%%
%% feed input into target array
%% and feed input as long as there input
%% each target trigger expression that reference
%% the vaiable onto a queue of things to evaluate.
%%
%% update Target' data with the new input
%% run eval with Target and Target' as input
%%
%%

do_input(_Type, Name, Value, Delta, Src, State) ->
    case dict:find(Name, State#state.targets) of
	error ->
	    lager:error("target ~s not found", [Name]),
	    {State#state.enable_value, 0, State};
	{ok,Target} ->
	    Value1 = if Delta =/= 0 ->
			     Value + target_in_value(Target, State);
			true ->
			     Value
		     end,
	    %% merge targets and core vars?
	    %% target could be declared as {x, "clamp(x)"}
	    %% or {y, "trunc(2*y + 1), even {z,"map(z,0,1,10,100)"}
	    {Value2,State1} = set_value(Target, Value1, Delta, State),
	    Core1 = hex_core:set_value(Name, Value1+Delta, State1#state.core),
	    Core2 = hex_core:eval(Core1),
	    Active =
	    case State#state.active_value of
		0 ->
		    case hex_core:value(State#state.active_var, Core2) of
			0 -> 0;
			_ -> transmit_active(1, State), 1
		    end;
		1 ->
		    case hex_core:value(State#state.active_var, Core2) of
			0 -> transmit_active(0, State), 0;
			_ -> 1
		    end
	    end,
	    Output = hex_core:value(State#state.output_var,Core2),
	    Env = [{Name,Value2},{output,Output},{source,Src}],
	    {Enabled,State2} =
	    case State#state.enable_value of
		0 ->
		    case hex_core:value(State#state.enable_var, Core2) of
			0 ->
			    {0,State1};
			_ ->
			    feedback(Output, State),
			    {1,action(Env,State1)}
		    end;
		1 ->
		    case hex_core:value(State#state.disable_var, Core2) of
			0 ->
			    feedback(Output, State),
			    {0,action(Env,State)};
			_ ->
			    feedback(Output, State),
			    {1,action(Env,State)}
		    end
	    end,
	    {Enabled, State2#state { enable_value = Enabled,
				     active_value = Active,
				     core = Core2 }}
    end.


set_value(Target, X, Delta, State) ->
    Xi = constrain_in_value(X + Delta, Target),
    Xo = map_value(Xi, Target),
    lager:debug("set_value target:~s delta:~w in:~w, constrained:~w, mapped:~w",
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

constrain_in_value(X, Target) when Target#target.type =:= wrap ->
    wrap_in_value(X, Target);
constrain_in_value(X, Target) when Target#target.type =:= clamp ->
    clamp_in_value(X, Target).

clamp_in_value(X, #target { in_min=X0, in_max=X1 }) ->
    if X1 >= X0 -> min(X1, max(X0, X));
       true -> min(X0, max(X1, X))
    end.

%% constrain_out_value(X, Target) when Target#target.type =:= wrap ->
%%     wrap_out_value(X, Target);
%% constrain_out_value(X, Target) when Target#target.type =:= clamp ->
%%     clamp_out_value(X, Target).

%% clamp_out_value(Y, #target { out_min=Y0, out_max=Y1 }) ->
%%    if Y1 >= Y0 -> min(Y1, max(Y0, Y));
%%       true -> min(Y0, max(Y1, Y))
%%    end.

wrap_in_value(X, #target { in_min=X0, in_max=X1 }) ->
    if X1 >= X0 ->
	    mod((X - X0),(X1 - X0 + 1)) + X0;
       X1 < X0 ->
	    mod((X - X1),(X0 - X1 + 1)) + X1
    end.

%% wrap_out_value(Y, #target { out_min=Y0, out_max=Y1 }) ->
%%    if Y1 >= Y0 ->
%%	    mod((Y - Y0),(Y1 - Y0 + 1)) + Y0;
%%       Y1 < Y0 ->
%%	    mod((Y - Y1),(Y0 - Y1 + 1)) + Y1
%%    end.

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
	active    -> #opt.active;
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
get_options([active|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{active,State#state.active}|Acc]);
get_options([chan|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{chan,State#state.chan}|Acc]);
get_options([ramp_min|Ks], Config, State, Acc) ->
    get_options(Ks, Config, State, [{ramp_min,State#state.ramp_min}|Acc]);
get_options([min_value|Ks], Config, State, Acc) ->
    #target { out_min=Min } = value_target(State),
    get_options(Ks, Config, State, [{min_value,Min}|Acc]);
get_options([max_value|Ks], Config, State, Acc) ->
    #target { out_max=Max } = value_target(State),
    get_options(Ks, Config, State, [{max_value,Max}|Acc]);
get_options([{target,Name}|Ks], Config, State, Acc) ->
    case dict:find(Name, State#state.targets) of
	error ->
	    get_options(Ks, Config, State, Acc);
	{ok,Target} ->
	    get_options(Ks, Config, State,
			[{target,[{name,Name},
				  {type,Target#target.type},
				  {in_min,Target#target.in_min},
				  {in_max,Target#target.in_max},
				  {out_min,Target#target.out_min},
				  {out_max,Target#target.out_max}]} |
			 Acc])
    end;
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

%% output "virtual" feedback
feedback(Value0, State) ->
    {Type,Value} = 
	if State#state.digital ->
		{?HEX_DIGITAL, if Value0>0 -> 1; true -> 0 end};
	   true ->
		{?HEX_ANALOG, Value0}
	end,
    Signal = #hex_signal { id=make_self(State#state.nodeid),
			   chan=State#state.chan,
			   type=Type,
			   value=Value,
			   source={output,State#state.chan}},
    Env = [],
    feedback_signal(Signal, Env, State),
    transmit_signal(Signal, Env, State).

action(Env, State) ->
    lager:debug("action env=~w\n", [alue,Env]),
    action_list(State#state.actions, Env, State).
    
action_list([{Cond,Action} | Actions], Env, State) ->
    case hex_core:value(Cond, State#state.core) of
	0 ->
	    lager:debug("eval ~p = 0\n", [Cond]),
	    action_list(Actions, Env, State);
	_CondVal ->
	    lager:debug("eval ~p = ~p\n", [Cond,_CondVal]),
	    execute(Action, Env),
	    action_list(Actions, Env, State)
    end;
action_list([], _Env, State) ->
    State.

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

%% util to handle some variants on actions
%% fixme: only allow one form!

rewrite_actions({App,Flags}) -> 
    [{{'!=',output,{const,0}},{App,Flags}}];
rewrite_actions([{Expr,{App,Flags}} | Actions]) ->
    Expr1 = if Expr =:= [] -> {const,1};
	       is_integer(Expr) -> {'==',value,{const,Expr}};
	       is_list(Expr) -> hex_core:parse(Expr)
	    end,
    [{Expr1,{App,Flags}} | rewrite_actions(Actions)];
rewrite_actions([]) ->
    [].
