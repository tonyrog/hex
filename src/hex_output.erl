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
%%%    Hex output output machine
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
	 state_activate/2,
	 state_reactivate/2,
	 state_delay/2,
	 state_sustain/2,
	 state_wait/2,
	 state_rampup/2,
	 state_rampdown/2,
	 state_deactivate/2,
	 state_deact/2]).
	 
-define(SERVER, ?MODULE).

-record(opt, {
	  type    = digital :: digital | analog | pulse,
	  nodeid  = 0 :: uint32(),    %% id of hex node
	  chan    = 0 :: uint8(),     %% output number 1..254	  
	  default = 0 :: uint32(),    %% default value 
	  inhibit = 0 :: timeout(),   %% ignore delay
	  delay   = 0 :: timeout(),   %% activation delay
	  rampup  = 0 :: timeout(),   %% analog ramp up time
	  rampdown = 0 :: timeout(),  %% analog ramp down time
	  sustain = 0 :: timeout(),   %% time to stay active
	  deact   = 0 :: timeout(),   %% deactivation delay
	  wait    = 0 :: timeout(),   %% delay between re-activation
	  repeat  = 0 :: integer(),   %% pulse repeat count
	  feedback = false :: boolean() %% feedback frames as input 
	 }).

-record(state, {
	  config  = #opt {} :: #opt{},   %% config values
	  value   = 0 :: uint32(),       %% current value
	  counter = 0 :: uint32(),       %% repeat counter
	  tref    = undefined :: undefined | reference(),
	  env     = [],                  %% enironment for last event
	  actions = []
	 }).


-define(STATE(Name), io:format("state: ~w\n", [(Name)])).

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
	    State = #state { value = Config#opt.default,
			     config = Config, 
			     actions = Actions },
	    {ok, state_off, State};
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
state_off(Event={digital,Val,Src}, State) when Val =/= 0 ->
    state_activate(Event, State#state { env = [{source,Src}] });
state_off({analog,Val,Src}, State) ->
    %% fixme: only in state_on?
    Env = [{value,Val}, {source,Src}],
    State1 = action(?HEX_ANALOG, Val, State#state.actions, Env, State),
    {next_state, state_off, State1};
state_off(_Event, State) ->
    {next_state, state_off, State}.

state_on({digital,0,Src}, State) ->
    state_deactivate(init, State#state { env = [{source,Src}] });
state_on({analog,Val,Src}, S) ->
    Env = [{value,Val}, {source,Src}],
    S1 = action(?HEX_ANALOG, Val, S#state.actions, Env, S),
    {next_state, state_on, S1};
state_on(_Event, S) ->
    {next_state, state_on, S}.

state_activate(Event, State) ->
    active(1, State),
    State1 = State#state { counter = (State#state.config)#opt.repeat },
    state_reactivate(Event, State1).

state_reactivate(Event, State) ->
    Time = (State#state.config)#opt.delay,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, delay),
	    State1 = State#state { tref = TRef },
	    {next_state, state_delay, State1};
       true ->
	    state_rampup(Event, State)
    end.

%% addme: accept {digital,0,_} to cancel activation in this phase!
state_delay({timeout,TRef,delay}, State) when State#state.tref =:= TRef ->
    State1 = State#state { tref = undefined },
    state_rampup(init, State1).

state_rampup(init, State) ->
    Time = (State#state.config)#opt.rampup,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, rampup),
	    State1 = State#state { tref = TRef },
	    {next_state, state_rampup, State1};
       true ->
	    state_sustain(init, State)
    end;
state_rampup({timeout,TRef,rampup}, State) when State#state.tref =:= TRef ->
    state_sustain(init, State#state { tref=undefined }).

state_sustain(init, State) ->
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
    state_deact(init, State#state { tref=undefined }).

state_deactivate(_Event, State) ->
    active(0, State),
    state_deact(init, State).

state_deact(init, State) ->
    Time = (State#state.config)#opt.deact,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, deact),
	    State1 = State#state { tref = TRef },
	    {next_state, state_deact, State1};
       true ->
	    state_rampdown(init, State)
    end;
state_deact({timeout,TRef,deact}, State) when State#state.tref =:= TRef ->
    state_rampdown(init, State#state { tref=undefined }).

state_rampdown(init, State) ->
    Time = (State#state.config)#opt.rampdown,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, rampdown),
	    State1 = State#state { tref = TRef },
	    {next_state, state_rampdown, State1};
       true ->
	    state_wait(init, State)
    end;
state_rampdown({timeout,TRef,rampdown}, State) when State#state.tref =:= TRef ->
    state_wait(init, State#state { tref=undefined }).

state_wait(init, State) ->
    Env = [{value,0}, State#state.env],
    State1 = action(?HEX_DIGITAL, 0, State#state.actions, Env, State),
    Time = (State1#state.config)#opt.wait,
    if Time > 0 ->
	    TRef = gen_fsm:start_timer(Time, wait),
	    State2 = State1#state { tref = TRef },
	    {next_state, state_wait, State2};
       true ->
	    active(0, State),
	    {next_state, state_off, State1}
    end;
state_wait({timeout,TRef,wait}, State) when State#state.tref =:= TRef ->
    Repeat = State#state.counter,
    if  Repeat =:= 0 ->
	    active(0, State),
	    {next_state, state_off, State#state { tref=undefined }};
	Repeat =:= -1 -> %% interval - pulse forever
	    state_reactivate(init, State#state { tref=undefined });
	Repeat > 0 ->  %% pulse counter
	    state_reactivate(init, State#state { tref=undefined,
						 counter = Repeat-1 })
    end.

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
	type when V =:= digital; V =:= analog; V =:= pulse -> 
	    Opt#opt  { type = V };
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
	feedback when is_boolean(V) -> Opt#opt { feedback = V }
    end.

make_self(NodeID) ->
    16#20000000 bor (2#0011 bsl 25) bor (NodeID band 16#1ffffff).

%% output activity on/off
active(Active, State) ->
    Opt = State#state.config,
    Signal = #hex_signal { id=make_self(Opt#opt.nodeid),
			   chan=Opt#opt.chan,
			   type=?HEX_OUTPUT_ACTIVE,
			   value=Active,
			   source={output,Opt#opt.chan}},
    Env = [],
    hex_server:transmit(Signal, Env).

	
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
