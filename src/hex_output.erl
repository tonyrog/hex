%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
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
	  wait    = 0 :: timeout(),    %% delay between re-activation
	  feedback = false :: boolean() %% feedback frames as input 
	 }).

-record(state, {
	  config  = #opt {} :: #opt{},    %% config values
	  value   = 0 :: uint32(),    %% current value
	  actions = []
	 }).

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
    gen_fsm:start_link(?MODULE, {Flags,Actions}, []).

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
	    S = #state { value = Config#opt.default,
			 config = Config, 
			 actions = Actions },
	    {ok, state_off, S};
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
state_off({digital,Val,_}, S) when Val =/= 0 ->
    S1 = action(?HEX_DIGITAL, 1, S#state.actions, S),
    {next_state, state_on, S1};
state_off(_Event, S) ->
    {next_state, state_off, S}.

state_on({digital,0,_}, S) ->
    S1 = action(?HEX_DIGITAL, 0, S#state.actions, S),
    {next_state, state_off, S1};
state_on(_Event, S) ->
    {next_state, state_on, S}.

state_activate(_Event, State) ->
    {next_state, state_activate, State}.

state_reactivate(_Event, State) ->
    {next_state, state_reactivate, State}.

state_delay(_Event, State) ->
    {next_state, state_delay, State}.

state_sustain(_Event, State) ->
    {next_state, state_sustain, State}.

state_wait(_Event, State) ->
    {next_state, state_rampup, State}.

state_rampup(_Event, State) ->
    {next_state, state_rampup, State}.

state_rampdown(_Event, State) ->
    {next_state, state_rampdown, State}.

state_deactivate(_Event, State) ->
    {next_state, state_deactivate, State}.

state_deact(_Event, State) ->
    {next_state, state_deact, State}.

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
	feedback when is_boolean(V) -> Opt#opt { feedback = V }
    end.
	

feedback(Type,Value, S) ->
    Opt = S#state.config,
    if Opt#opt.feedback ->
	    hex_server ! #hex_signal { id=Opt#opt.nodeid,
				       chan=Opt#opt.chan,
				       type=Type,
				       value=Value,
				       source={output,Opt#opt.chan}};
       true ->
	    ok
    end.


action(Type, Value, Action, S) ->
    feedback(Type, Value, S),
    if is_list(Action) ->
	    action_list(Value, Action, S);
       true ->
	    action_list(Value, [{Value,Action}], S)
    end.

action_list(Value, [{Match,Action} | Actions], S) ->
    case hex_server:match_value(Match, Value) of
	true ->
	    execute(Action, [{value,Value}]),
	    action_list(Value, Actions, S);
	false ->
	    action_list(Value, Actions, S)
    end;
action_list(_Value, [], S) ->
    S.

execute({App, AppFlags}, Env) ->
    try App:output(AppFlags, Env) of
	Result ->
	    io:format("execute ~p = ~p\n", [{App,AppFlags}, Result]),
	    Result
    catch
	error:Reason ->
	    io:format("execute ~p = ~p\n", [{App,AppFlags}, {error,Reason}]),
	    {error,Reason}
    end.
