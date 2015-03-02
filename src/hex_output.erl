%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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

%%
%% note!  a short pulse must be at least 1 ms long, since
%% sustain = 0 means that output goes into state_on.
%%
-define(DICT_T, term()).  %% until the dust settles

-type core_var() :: atom() | integer().

-record(var, {
	  in       :: core_var(),
	  an       :: core_var(),
	  out      :: core_var(),
	  ena      :: core_var(),
	  low      :: core_var(),
	  high     :: core_var(),
	  inhibit  :: core_var(),
	  delay    :: core_var(),   %% activation delay
	  rampup   :: core_var(),   %% ramp up time
	  sustain  :: core_var(),   %% time to stay active
	  rampdown :: core_var(),   %% ramp down time
	  deact    :: core_var(),   %% deactivation delay
	  wait     :: core_var(),   %% delay between re-activation
	  repeat   :: core_var(),   %% pulse repeat count
	  feedback :: core_var(),   %% feedback frames as input
	  transmit :: core_var(),   %% transmit frames
	  enable   :: core_var(),   %% "in"
	  disable  :: core_var(),   %% "!in"
	  active   :: core_var(),   %% "in"
	  inactive :: core_var(),   %% "!in"
	  output   :: core_var(),   %% "out"
	  enabled  :: core_var()    %% "ena"
	 }).

-record(state, {
	  nodeid  = 0 :: uint32(),         %% id of hex node
	  chan    = 0 :: uint8(),          %% output number 1..254
	  ramp_step = 20 :: uint32(),       %% min time quanta in during ramp

	  counter = 0 :: uint32(),         %% repeat counter
	  tref    = undefined :: undefined | reference(),
	  tramp   = undefined :: undefined | reference(),

	  var  :: #var {},                    %% core variables
	  analog  = false,                    %% output analog values
	  deactivate = false :: boolean(),    %% set while deactivate
	  inhibited  = false :: boolean(),    %% disallow activation

	  out_name  = out :: atom(),          %% name of output variable
	  ena_name  = ena :: atom(),          %% enabled output variabled

	  enable_value = 0,                   %% last enable value
	  active_value = 0,                   %% last active value

	  core = undefined :: term(),         %% core structure
	  targets = undefined :: ?DICT_T,     %% declared variables

	  env     = [],                       %% enironment for last event
	  actions = []                        %% [{Cond,Value,{App,Flags}}]
	 }).

-ifdef(state_debug).
-define(STATE(Name),
	io:format("~s:~w: ~p STATE: ~w\n",
		  [?MODULE, ?LINE, State#state.chan, (Name)])).
-define(STATE(Name,Event),
	io:format("~s:~w: ~p STATE: ~w event: ~p\n",
		  [?MODULE, ?LINE,
		   State#state.chan,
		   (Name),(Event)])).
-define(verbose(Fmt,As), io:format((Fmt)++"\n", (As))).
-else.
-define(STATE(Name), ok).
-define(STATE(Name,Event), ok).
-define(verbose(Fmt,As), ok).
-endif.

-define(INPUT_NONE,     0).
-define(INPUT_ENABLE,   1).
-define(INPUT_DISABLE, -1).

-define(CORE_VALUE(St,Core,Var), hex_core:value(((St)#state.var)#var.Var,
						(Core))).

-define(VALUE(St,Var), hex_core:value(((St)#state.var)#var.Var,
				      (St)#state.core)).

%%%===================================================================
%%% API
%%%===================================================================

setopts(Pid, Flags) ->
    gen_fsm:sync_send_all_state_event(Pid, {setopts,Flags}).

getopts(Pid, Flags) ->
    gen_fsm:sync_send_all_state_event(Pid, {getopts,Flags}).

validate_flags(Flags) ->
    hex:validate_flags(Flags, event_spec()).

event_spec() ->
    [
     {leaf, nodeid, [{type, uint32, []},
		     {default, 0, []}]},
     {leaf, chan, [{type, uint8, []},
		   {default, 0, []}]},
     {leaf, ramp_step, [{type, uint32, []},
			{default, 20, []},
			{description, "The min step in ms for each output "
			 "change in ramp -up or -down.", []}
		       ]},
     {leaf, input, [{type,uint32, []},
		    {default, 0, []},
		    %% {config, false, []},
		    {description, "The input value", []}
		   ]},
     {leaf, low, [{type,uint32, []},
			{default, 0, []},
			{description, "set low value level", []}
		       ]},
     {leaf, high, [{type,uint32, []},
			{default, 16#ffff, []},
			{description, "same high value level", []}
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
     {leaf,repeat, [{type, int32,
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
		   {default,"in", []},
		   {description, "Boolean expression describing "
		    "when an output is enabled. That is, "
		    "the output is unlatched."}
		  ]},
     {leaf,disable,[{type,string,[]},
		    {default,"!in", []},
		    {description, "Boolean expression describing "
		     "when an output is disabled, that is the output is "
		     "latched.", []}
		   ]},
     {leaf,active, [{type,string,[]},
		    {default,"in", []},
		    {description, "Boolean expression describing when an "
		     "activation signal will be sent.",[]}
		   ]},
     {leaf,inactive, [{type,string,[]},
		      {default,"not in", []},
		      {description, "Boolean expression describing when an "
		       "deactivation signal will be sent.",[]}
		     ]},
     {leaf,output, [{type,string,[]},
		    {default,"out", []},
		    {description, "Expression to calculate the output value.",
		    []}
		   ]},

     {leaf,enabled, [{type,string,[]},
		     {default,"ena", []},
		     {description, "Expression to calculate the enabled value.",
		      []}
		    ]},

     {leaf,out_name, [{type,string,[]},
		      {default,"out", []},
		      {description, "Name of the output variable.", []}
		     ]},

     {leaf,ena_name, [{type,string,[]},
		      {default,"ena", []},
		      {description, "Name of the enabled variable.", []}
		     ]},

     {leaf,analog, [{type,boolean,[]},
		    {default,false,[]},
		     {description, "if 'analog' is true then an analog "
		      "signal is sent in transmit or feedback otherwise "
		      "only a digital signal is sent.",
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
       {leaf, expr, [{type,string,[]}]}
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
    Core0 = hex_core:new(),
    Targets0 = dict:new(),

    %% "standard" core variables
    {[Var_in,Var_an,Var_ena,Var_out,
      Var_low,Var_high,Var_inhibit,
      Var_delay,Var_rampup,Var_sustain,Var_rampdown,
      Var_deact,Var_wait,Var_repeat,Var_feedback,Var_transmit,
      Var_enable,Var_disable,Var_active,Var_inactive,
      Var_output,Var_enabled
     ],Core1,Targets1} =
	install_variables(
	  [{in,0},
	   {an,0},
	   {ena,0},
	   {out,0},
	   {low,0},
	   {high,16#ffff},
	   {inhibit,0},
	   {delay,0},
	   {rampup,0},
	   {sustain,0},
	   {rampdown,0},
	   {deact,0},
	   {wait,0},
	   {repeat,0},
	   {feedback,0},
	   {transmit,0},
	   {enable,"in"},
	   {disable,"!in"},
	   {active,"in"},
	   {inactive, "!in"},
	   {output, "out"},
	   {enabled, "ena"}
	  ],
	  Core0, Targets0, []),

    Var = #var {
	     in=Var_in,
	     an=Var_an,
	     ena=Var_ena,
	     out=Var_out,
	     low=Var_low,
	     high=Var_high,
	     inhibit=Var_inhibit,
	     delay=Var_delay,
	     rampup=Var_rampup,
	     sustain=Var_sustain,
	     rampdown=Var_rampdown,
	     deact=Var_deact,
	     wait=Var_wait,
	     repeat=Var_repeat,
	     feedback=Var_feedback,
	     transmit=Var_transmit,
	     enable=Var_enable,
	     disable=Var_disable,
	     active=Var_active,
	     inactive=Var_inactive,
	     output=Var_output,
	     enabled=Var_enabled
	    },
    Actions = rewrite_actions(Actions0),
    State0 = install_actions(Actions, #state { var = Var,
					       core= Core1,
					       targets=Targets1 }),
    %% fixme: transmit input mapping?
    %% fixme: order of install actions and set_options?
    case set_options(Flags, State0) of
	{ok, State} ->
	    Core2 = hex_core:eval(State#state.core),
	    A = min(?CORE_VALUE(State,Core2,active), 1),
	    transmit_active(A, State),
	    E = min(?CORE_VALUE(State,Core2,enable), 1),
	    if E =:= 0 ->
		    ?STATE(off),
		    State2 = State#state { enable_value=E,
					   active_value=A,
					   deactivate=true,
					   core=Core2 },
		    {_,_,State3} = state_off(init, State2),
		    {ok, state_off, State3};
	       E =:= 1 ->
		    ?STATE(on),
		    State2 = do_enabled(1, State#state { enable_value=E,
							 active_value=A,
							 core=Core2 }),
		    {_, _, State3} = state_on(init, State2),
		    {ok, state_on, State3}
	    end;
	Error ->
	    {stop, Error}
    end.

install_variables([{Var,Value}|Vs], Core, Targ, Acc) when is_atom(Var) ->
    if is_number(Value) ->
	    {Vi,Core1} = hex_core:set_value(Var,Value,Core),
	    Targ1 = dict:store(Var,Vi,Targ),
	    install_variables(Vs, Core1, Targ1, [Vi|Acc]);
       is_list(Value) ->
	    try hex_core:parse(Value) of
		{ok,Expr} ->
		    {Vi,Core1} = hex_core:add_expr(Expr,Core),
		    Targ1 = dict:store(Var,Vi,Targ),
		    install_variables(Vs, Core1, Targ1, [Vi|Acc]);
		Error ->
		    Error
	    catch
		error:Reason ->
		    {error, {badarg,{Var,Reason}}}
	    end
    end;
install_variables([], Core, Targ, Acc) ->
    {lists:reverse(Acc), Core, Targ}.

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
state_off(init, State) ->
    ?STATE(off),
    State1 = do_enabled(0, State),
    _Remained = cancel_timer(State1#state.tref),
    if State1#state.active_value =:= 1, not State1#state.deactivate ->
	    transmit_active(0, State);
       true -> %% already sent
	    ok
    end,
    lager:debug("action in state_off init\n"),
    State2 = action(State1),
    {next_state, state_off, State2#state { tref=undefined,
					   active_value = 0,
					   enable_value = 0,
					   deactivate=false}};

state_off(Event={Name,{_Type,Value,_Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(off, Event),
    if State#state.inhibited ->
	    ?verbose("inhibited in state_off", []),
	    {next_state, state_off, State};
       true ->
	    case do_input(Event, State) of
		{?INPUT_ENABLE,State1} ->
		    do_activate(Event, State1);
		{_, State1} -> %% disable | none
		    {next_state, state_off, State1}
	    end
    end;
state_off(_Event, State) ->
    lager:debug("ignore event ~p", [_Event]),
    {next_state, state_off, State}.

state_on(init, State) ->
    ?STATE(on),
    lager:debug("action in state_in init\n"),
    State1 = action(State),
    {next_state, state_on, State1};
state_on(Event={Name,{_Type,Value,_Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(on, Event),
    case do_input(Event, State) of
	{?INPUT_DISABLE, State1} ->
	    do_deactivate(init, State1);
	{_, State1} ->
	    {next_state, state_on, State1}
    end;
state_on(_Event, S) ->
    lager:debug("ignore event ~p", [_Event]),
    {next_state, state_on, S}.

%% activation delay, activation may be cancelled in this phase
%% by sending an digital 0 signal
state_delay(init, State) ->
    ?STATE(delay),
    Time = ?VALUE(State, delay),
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, delay),
	    State1 = State#state { tref = TRef },
	    {next_state, state_delay, State1};
       true ->
	    state_rampup(init, State)
    end;
state_delay(_Event={timeout,TRef,delay}, State)
  when State#state.tref =:= TRef ->
    ?STATE(delay, _Event),
    State1 = State#state { tref = undefined },
    state_rampup(init, State1);
state_delay(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(delay, Event),
    case do_input(Event, State) of
	{?INPUT_DISABLE, State1} ->
	    lager:debug("activation cancelled during delay from ~p", [Src]),
	    state_off(init, State1);
	{_,State1} ->
	    {next_state, state_delay, State1}
    end;
state_delay(_Event, State) ->
    lager:debug("state_delay: ignore event ~p", [_Event]),
    {next_state, state_delay, State}.

%% Ramping up output over rampup time milliseconds
state_rampup(init, State) ->
    ?STATE(rampup),
    Time = ?VALUE(State, rampup),
    if Time > 0 ->
	    Tm = State#state.ramp_step,
	    A0 = ?VALUE(State, low),
	    A1 = ?VALUE(State, high),
	    A = ?VALUE(State,output),
	    %% A = hex_core:value(State#state.out_name, State#state.core),
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
	    state_sustain(init, State)
    end;
state_rampup(_Event={timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    ?STATE(rampup, _Event),
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampup, State#state { tref=undefined} };
	Tr ->
	    Time = ?VALUE(State, rampup),
	    A0 = ?VALUE(State, low),
	    A1 = ?VALUE(State, high),
	    T = (Time - Tr)/Time,
	    A = trunc((A1-A0)*T + A0),
	    ?verbose("rampup: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampup, State2}
    end;
state_rampup(_Event={timeout,TRef,done}, State)
  when State#state.tramp =:= TRef ->
    ?STATE(rampup, _Event),
    cancel_timer(State#state.tref),
    State1 = State#state { tramp=undefined, tref=undefined },
    ?verbose("rampup: T=~w A=~w", [1.0, max]),
    State2 = do_analog_value(max,State1),
    state_sustain(init, State2);

state_rampup(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(rampup, Event),
    case do_input(Event, State) of
	{?INPUT_DISABLE,State1} ->
	    lager:debug("rampup cancelled from ~p", [Src]),
	    cancel_timer(State1#state.tref),
	    cancel_timer(State1#state.tramp),
	    do_deactivate(Event, State1#state { tref=undefined,
						tramp=undefined });
	{_,State1} ->
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
    Time = ?VALUE(State, sustain),
    State1 = do_analog_value(max,State),
    if Time > 0 ->
	    lager:debug("action in state_sustain init\n"),
	    State2 = action(State1),
	    TRef = gen_fsm:start_timer(Time, sustain),
	    State3 = State2#state { tref = TRef },
	    {next_state, state_sustain, State3};
       true ->
	    state_on(init, State1)
    end;
state_sustain(_Event={timeout,TRef,sustain}, State)
  when State#state.tref =:= TRef ->
    ?STATE(sustain, _Event),
    state_deact(init, State#state { tref=undefined });

state_sustain(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(sustain, Event),
    case do_input(Event, State) of
	{?INPUT_DISABLE,State1} ->
	    lager:debug("sustain cancelled from ~p", [Src]),
	    gen_fsm:cancel_timer(State1#state.tref),
	    do_deactivate(init, State1#state { deactivate=true,
					       tref = undefined });
	{?INPUT_NONE,State1} ->
	    lager:debug("action in state_sustain event\n"),
	    State2 = action(State1),
	    {next_state, state_sustain, State2};
	{?INPUT_ENABLE,State1} ->
	    {next_state, state_sustain, State1}
    end;
state_sustain(_Event, S) ->
    lager:debug("state_sustain: ignore event ~p", [_Event]),
    {next_state, state_sustain, S}.

%% deactivation delay
state_deact(init, State) ->
    ?STATE(deact),
    Time = ?VALUE(State, deact),
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
    ?STATE(deact, Event),
    if State#state.inhibited ->
	    ?verbose("inhibited in state_deact", []),
	    {next_state, state_deact, State};
       true ->
	    case do_input(Event, State) of
		{?INPUT_ENABLE, State1} ->
		    lager:debug("deact cancelled from ~p", [Src]),
		    _Remain = gen_fsm:cancel_timer(State#state.tref),
		    %% FIXME: calculate remain sustain?
		    %% or think if this as reactivation?
		    state_sustain(init, State1#state { tref = undefined,
						       deactivate=false });
		{_, State1} ->
		    {next_state, state_deact, State1}
	    end
    end;
state_deact(_Event, S) ->
    lager:debug("state_deact: ignore event ~p", [_Event]),
    {next_state, state_deact, S}.

%% Ramping down output over rampup time milliseconds
state_rampdown(init, State) ->
    ?STATE(rampdown),
    Time = ?VALUE(State, rampdown),
    if Time > 0 ->
	    Tm = State#state.ramp_step,
	    A0 = ?VALUE(State, low),
	    A1 = ?VALUE(State, high),
	    A = ?VALUE(State,output),
	    %% A = hex_core:value(State#state.out_name, State#state.core),
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
	    state_wait(init, State)
    end;
state_rampdown(_Event={timeout,TRef,{delta,Td}},State)
  when State#state.tref =:= TRef ->
    ?STATE(rampdown,_Event),
    case erlang:read_timer(State#state.tramp) of
	false ->
	    {next_state, state_rampdown, State#state {tref=undefined} };
	Tr ->
	    Time = ?VALUE(State, rampdown),
	    A0 = ?VALUE(State, low),
	    A1 = ?VALUE(State, high),
	    T = (Time-Tr)/Time,
	    A = trunc((A0-A1)*T + A1),
	    ?verbose("rampdown: T=~w A=~w", [T, A]),
	    TRef1 = gen_fsm:start_timer(Td, {delta,Td}),
	    State1 = State#state { tref = TRef1 },
	    State2 = do_analog_value(A,State1),
	    {next_state, state_rampdown, State2}
    end;
state_rampdown(_Event={timeout,TRef,done}, State) when
      State#state.tramp =:= TRef ->
    ?STATE(rampdown,_Event),
    cancel_timer(State#state.tref),
    State1 = State#state { tramp=undefined, tref=undefined },
    ?verbose("rampdown: T=~w A=~w", [1.0, min]),
    State2 = do_analog_value(min,State1),
    state_wait(init, State2);
state_rampdown(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(rampdown,Event),
    if State#state.inhibited ->
	    ?verbose("inhibited in state_rampdown", []),
	    {next_state, state_rampdown, State};
       true ->
	    case do_input(Event, State) of
		{?INPUT_DISABLE, State1} ->
		    lager:debug("deactivate during rampdown from ~p", [Src]),
		    %% mark for deactivation - when ramp is done
		    {next_state, state_rampdown, State1#state { deactivate=true }};
		{?INPUT_ENABLE, State1} ->
		    lager:debug("rampdown cancelled from ~p", [Src]),
		    _Remain = gen_fsm:cancel_timer(State1#state.tref),
		    state_rampup(init, State1#state { tref = undefined,
						      deactivate=false });
		{?INPUT_NONE, State1} ->
		    {next_state, state_rampdown, State1}
	    end
    end;
state_rampdown(_Event, State) ->
    lager:debug("state_rampdown: ignore event ~p", [_Event]),
    {next_state, state_rampdown, State}.

state_wait(init, State) ->
    ?STATE(wait),
    State1 = do_analog_value(min,State),
    WaitTime = ?VALUE(State1, wait),
    if State1#state.enable_value =:= 0; State1#state.counter =:= 0 ->
	    state_off(init, State1);
       State1#state.deactivate ->
	    state_off(init, State1);
       true ->
	    %% context switch even if WaitTime = 0!
	    TRef = gen_fsm:start_timer(WaitTime, wait),
	    State2 = State1#state { tref = TRef },
	    {next_state, state_wait, State2}
    end;
state_wait(_Event={timeout,TRef,wait}, State)
  when State#state.tref =:= TRef ->
    ?STATE(wait, _Event),
    Repeat = State#state.counter,
    if  Repeat =:= 0 -> %% should not happend?
	    state_off(init, State#state { tref=undefined} );
	Repeat =:= -1 -> %% interval - pulse forever
	    do_reactivate(init, State#state { tref=undefined });
	Repeat > 0 ->  %% pulse counter
	    do_reactivate(init, State#state { tref=undefined,
					      counter = Repeat-1 })
    end;

state_wait(Event={Name,{_Type,Value,Src}}, State) when
      is_atom(Name), is_integer(Value) ->
    ?STATE(wait, Event),
    case do_input(Event, State) of
	{?INPUT_DISABLE, State1} ->
	    lager:debug("disable during wait from ~p", [Src]),
	    state_off(init, State1);
	{_, State1} ->
	    {next_state, state_wait, State1}
    end;
state_wait(_Event, State) ->
    lager:debug("state_wait: ignore event ~p", [_Event]),
    {next_state, state_wait, State}.

do_activate(Event, State) ->
    ?verbose("DO_ACTIVATE",[]),
    Repeat = ?VALUE(State, repeat),
    State1 = State#state { counter = Repeat },
    do_reactivate(Event, State1).

do_reactivate(_Event, State) ->
    ?verbose("DO_REACTIVATE",[]),
    state_delay(init, State).
    %% state_rampup(init, State).

do_deactivate(_Event, State) ->
    ?verbose("DO_DEACTIVATE",[]),
    state_deact(init, State).

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
    try get_options(Flags, State, []) of
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
set_options([Opt|Opts], State) ->
    case Opt of
	{nodeid,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { nodeid=Value },
		    set_options(Opts, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{out_name,Name} when is_atom(Name) ->
	    State1 = State#state { out_name = Name },
	    set_options(Opts, State1);

	{ena_name,Name} when is_atom(Name) ->
	    State1 = State#state { ena_name = Name },
	    set_options(Opts, State1);

	{analog,Value} when is_boolean(Value) ->
	    State1 = State#state { analog = Value },
	    set_options(Opts, State1);

	{chan,Value} ->
	    if ?is_uint8(Value) ->
		    State1 = State#state { chan=Value },
		    set_options(Opts, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{ramp_step,Value} ->
	    if ?is_uint32(Value) ->
		    State1 = State#state { ramp_step=min(20,Value) },
		    set_options(Opts, State1);
	       true ->
		    {error, {badarg,Opt}}
	    end;

	{target,Value} ->
	    Name = proplists:get_value(name, Value, undefined),
	    Expr = proplists:get_value(expr, Value, undefined),
	    if Name =:= undefined ->
		    {error, {badarg, Opt}};
	       is_atom(Name), is_number(Expr) ->
		    {Vi,Core1}=hex_core:set_value(Name,Expr,State#state.core),
		    Targets = dict:store(Name,Vi,State#state.targets),
		    set_options(Opts, State#state { core = Core1,
						       targets = Targets });
	       is_atom(Name), is_list(Expr) ->
		    try hex_core:parse(Expr) of
			{ok,E} ->
			    {Vi,Core1} = hex_core:add_expr(E,State#state.core),
			    Targets = dict:store(Name,Vi,State#state.targets),
			    State1=State#state { core=Core1,
						 targets=Targets },
			    set_options(Opts, State1);
			{error,Reason} ->
			    {error, {Name,Reason}}
		    catch
			error:_Reason ->
			    {error, {badarg,Name}}
		    end;
	       is_atom(Name), Expr =:= undefined ->
		    {Vi,Core1}=hex_core:set_value(Name,{const,0},
						  State#state.core),
		    Targets = dict:store(Name,Vi,State#state.targets),
		    set_options(Opts, State#state { core = Core1,
						    targets = Targets });
	       true ->
		    {error, {badarg, Opt}}
	    end;

	{Name,Value} ->
	    case Name of
		in -> set_option(in, #var.in, Value, Opts, State);
		an -> set_option(an, #var.an, Value, Opts, State);
		out -> set_option(out, #var.out, Value, Opts, State);
		ena -> set_option(ena, #var.ena, Value, Opts, State);
		low -> set_option(low, #var.low, Value, Opts, State);
		high -> set_option(high, #var.high, Value, Opts, State);
		inhibit -> set_option(inhibit, #var.inhibit, Value, Opts, State);
		delay -> set_option(delay, #var.delay, Value, Opts, State);
		rampup -> set_option(rampup, #var.rampup, Value, Opts, State);
		sustain -> set_option(sustain, #var.sustain, Value, Opts, State);
		rampdown -> set_option(rampdown, #var.rampdown, Value, Opts, State);
		deact -> set_option(deact, #var.deact, Value, Opts, State);
		wait -> set_option(wait, #var.wait, Value, Opts, State);
		repeat -> set_option(repeat, #var.repeat, Value, Opts, State);
		feedback -> set_option(feedback, #var.feedback, Value, Opts, State);
		transmit -> set_option(transmit, #var.transmit, Value, Opts, State);
		enable -> set_option(enable, #var.enable, Value, Opts, State);
		disable -> set_option(disable, #var.disable, Value, Opts, State);
		active -> set_option(active, #var.active, Value, Opts, State);
		inactive -> set_option(inactive, #var.inactive, Value, Opts, State);
		output -> set_option(output, #var.output, Value, Opts, State);
		enabled -> set_option(enabled, #var.enabled, Value, Opts, State);
		_ ->
		    {error, {badarg, Name}}
	    end;
	_ ->
	    {error, {badarg,Opt}}
    end;
set_options([], State) ->
    {ok, State}.

set_option(Name, VarPos, true, Opts, State) ->
      set_option(Name, VarPos, 1, Opts, State);
set_option(Name, VarPos, false, Opts, State) ->
      set_option(Name, VarPos, 0, Opts, State);
set_option(Name, VarPos, Expr, Opts, State) when
      is_atom(Name), is_number(Expr) ->
    {Vi,Core1}=hex_core:set_value(Name,Expr,State#state.core),
    Var = setelement(VarPos, State#state.var, Vi),
    set_options(Opts, State#state { core = Core1, var=Var });
set_option(Name, VarPos, Expr, Opts, State) when
      is_atom(Name), is_list(Expr) ->
    try hex_core:parse(Expr) of
	{ok,E} ->
	    {Vi,Core1} = hex_core:add_expr(E,State#state.core),
	    Var = setelement(VarPos, State#state.var, Vi),
	    State1=State#state { core=Core1, var=Var },
	    set_options(Opts, State1);
	{error,Error} ->
	    {error, {Name, Error}}
    catch
	error:_Reason ->
	    {error, {badarg,Name}}
    end;
set_option(Name, _VarPos, _Expr, _Opts, _State) ->
    {error, {badarg,Name}}.

do_input({Name,{digital,Value,Src}}, State) ->
    do_value(?HEX_DIGITAL,Name, Value, Src, State);
do_input({Name,{analog,Value,Src}}, State) ->
    do_value(?HEX_ANALOG,Name, Value, Src, State);
do_input({Name,{encoder,Delta,Src}}, State) ->
    do_delta(?HEX_ANALOG,Name, Delta, Src, State);
do_input({Name,{?HEX_DIGITAL,Value,Src}}, State) ->
    do_value(?HEX_DIGITAL,Name, Value, Src, State);
do_input({Name,{?HEX_ANALOG,Value,Src}}, State) ->
    do_value(?HEX_ANALOG,Name, Value, Src, State);
do_input({Name,{?HEX_ENCODER,Delta,Src}}, State) ->
    do_delta(?HEX_ANALOG,Name, Delta, Src, State);
do_input({Name,{?HEX_OUTPUT_ACTIVE,Value,Src}}, State) ->
    do_value(?HEX_DIGITAL,Name, Value, Src, State);
do_input(_Event, State) ->
    lager:debug("ignore event ~p", [_Event]),
    {State#state.enable_value, State}.

do_delta(Type, Name, Delta, Src, State) ->
    lager:debug("do_delta: ~s = ~w delta=~w, src=~w", [Name,Delta,Src]),
    case dict:find(Name, State#state.targets) of
	error ->
	    lager:error("target ~s not declared", [Name]),
	    {?INPUT_NONE, State};
	{ok,Vo} ->
	    try hex_core:value(Name, State#state.core) of
		Value ->
		    do_target(Type,Name, Vo, Delta+Value, Src, State)
	    catch
		error:_ ->
		    lager:error("target ~s not in core", [Name]),
		    {?INPUT_NONE, State}
	    end
    end.

do_value(Type, Name, Value, Src, State) ->
    lager:debug("do_value: ~s = ~w src=~w", [Name,Value,Src]),
    case dict:find(Name, State#state.targets) of
	error ->
	    {?INPUT_NONE, State};
	{ok,Vo} ->
	    do_target(Type, Name, Vo, Value, Src, State)
    end.

do_target(_Type, Name, _Vo, Value, Src, State) ->
    lager:debug("do_target: ~s = ~w src=~w", [Name,Value,Src]),
    {_,Core1} = hex_core:set_value(Name,Value,State#state.core),
    Core2 = hex_core:eval(Core1),
    %% Value2 = hex_core:value(Vo,Core2),
    Active =
	case State#state.active_value of
	    0 ->
		case ?CORE_VALUE(State,Core2,active) of
		    0 -> 0;
		    _ -> transmit_active(1, State), 1
		end;
	    1 ->
		case ?CORE_VALUE(State,Core2,inactive) of
		    0 -> transmit_active(0, State), 0;
		    _ -> 1
		end
	end,
    PrevEnabled = State#state.enable_value,
    Enabled =
	case PrevEnabled of
	    0 ->
		min(?CORE_VALUE(State,Core2,enable), 1);
	    1 ->
		1-min(?CORE_VALUE(State,Core2,disable),1)
	end,
    %% FIXME: configure a flexible environment ????
    State1 = State#state { enable_value = Enabled,
			   active_value = Active,
			   core = Core2 },
    State2 = State1#state { env = [{source,Src}|target_env(State1)]},

    if not State#state.inhibited, Enabled =:= 1, PrevEnabled =:= 0 ->
	    State3 = start_inhibation(State2),
	    {?INPUT_ENABLE, do_enabled(1, State3)};
       Enabled =:= 0, PrevEnabled =:= 1 ->
	    {?INPUT_DISABLE, do_enabled(0, State2)};
       true ->
	    {?INPUT_NONE, State2}
    end.

target_env(State) ->
    dict:fold(fun(Ti,To,Acc) ->
		      Val = hex_core:value(To,State#state.core),
		      [{Ti,Val}|Acc]
	      end, [], State#state.targets).

do_enabled(Value, State) ->
    Core1 = hex_core:set_values([{State#state.out_name,Value},
				 {State#state.ena_name,Value}],
				State#state.core),
    Core2 = hex_core:eval(Core1),
    State1 = State#state { core=Core2 },
    Output = ?VALUE(State1, output),
    feedback(?HEX_DIGITAL, Output, State1),
    State1.

do_analog_value(min, State) ->
    do_analog_value(?VALUE(State,low), State);
do_analog_value(max, State) ->
    do_analog_value(?VALUE(State,high), State);
do_analog_value(A, State) when is_integer(A) ->
    lager:debug("do_analog_value: value = ~w\n", [A]),
    if State#state.analog ->
	    do_output(A, State);
       true ->
	    State
    end.

%% output analog values and run actions
do_output(Value, State) ->
    lager:debug("do_output value=~w", [Value]),
    if State#state.enable_value =:= 1 ->
	    {_,Core1} = hex_core:set_value(State#state.out_name, Value,
					   State#state.core),
	    Core2 = hex_core:eval(Core1),
	    Output = ?CORE_VALUE(State,Core2,output),
	    feedback(?HEX_ANALOG, Output, State),
	    Env = [{State#state.out_name, Output},
		   {source, {output,State#state.chan}}],
	    action(State#state { env=Env, core=Core2 } );
       true ->
	    State
    end.

start_inhibation(State) when not State#state.inhibited ->
    Inhibit = ?VALUE(State, inhibit),
    if Inhibit > 0 ->
	    erlang:start_timer(Inhibit, self(), inhibit_done),
	    ?verbose("inhibit started for: ~w ms", [Inhibit]),
	    State#state { inhibited = true };
       true ->
	    State
    end;
start_inhibation(State) ->
    State.

%% cancel a gen_fsm timer, if set.
cancel_timer(undefined) ->
    0;
cancel_timer(TRef) ->
    gen_fsm:cancel_timer(TRef).


get_options([nodeid|Ks], State, Acc) ->
    get_options(Ks, State, [{nodeid,State#state.nodeid}|Acc]);
get_options([chan|Ks], State, Acc) ->
    get_options(Ks, State, [{chan,State#state.chan}|Acc]);
get_options([ramp_step|Ks], State, Acc) ->
    get_options(Ks, State, [{ramp_step,State#state.ramp_step}|Acc]);
get_options([K|Ks], State, Acc) ->
    get_options(Ks, State, [{K,get_option(K,State)}|Acc]);
get_options([], _State, Acc) ->
    lists:reverse(Acc).

get_option(Key, State) ->
    case dict:is_key(Key, State#state.targets) of
	true ->
	    try hex_core:value(Key, State#state.core) of
		Value -> Value
	    catch
		error:R -> {error,R}
	    end;
	false ->
	    {error,key_not_declared}
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

feedback_signal(Signal, Env, State) ->
    case ?VALUE(State, feedback) of
	0 -> ok;
	_ -> hex_server:event(Signal, Env)
    end.

transmit_signal(Signal, Env, State) ->
    case ?VALUE(State, feedback) of
	0 -> ok;
	_ -> hex_server:transmit(Signal, Env)
    end.

%% output "virtual" feedback
feedback(Type, Value, State) ->
    Signal = #hex_signal { id=make_self(State#state.nodeid),
			   chan=State#state.chan,
			   type=Type,
			   value=Value,
			   source={output,State#state.chan}},
    Env = [],
    feedback_signal(Signal, Env, State),
    transmit_signal(Signal, Env, State).

action(State) ->
    action(State#state.env, State).

action(Env, State) ->
    lager:debug("action env=~w\n", [Env]),
    action_list(State#state.actions, Env, State).

action_list([{Cond,Action} | Actions], Env, State) ->
    case hex_core:value(Cond, State#state.core) of
	0 ->
	    lager:debug("eval var ~p = 0\n", [Cond]),
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
    [{{'/=',output,{const,0}},{App,Flags}}];
rewrite_actions([{Expr,{App,Flags}} | Actions]) ->
    Expr1 = if Expr =:= [] -> {const,1};
	       is_integer(Expr) -> {'==',value,{const,Expr}};
	       is_list(Expr) -> 
		    %% fixme: handle parse error etc
		    {ok,E1} = hex_core:parse(Expr),
		    E1
	    end,
    [{Expr1,{App,Flags}} | rewrite_actions(Actions)];
rewrite_actions([]) ->
    [].

install_actions(Actions, State) ->
    install_actions_(Actions, State#state.core, [], State).

install_actions_([{Expr,AppFlags}|Actions], Core, Acc, State) ->
    {Var,Core1} = hex_core:add_expr(Expr, Core),
    install_actions_(Actions, Core1, [{Var,AppFlags}|Acc], State);
install_actions_([], Core, Acc, State) ->
    State#state { core = Core,
		  actions = lists:reverse(Acc) }.
