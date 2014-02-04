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

%% API
-export([start_link/1]).

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

-type uint32() :: 0..16#ffffffff.

-record(state, {
	  type    = digital :: digital | analog | pulse,
	  value   = 0 :: uint32(),    %% current value
	  default = 0 :: uint32(),    %% default value 
	  inhibit = 0 :: timeout(),   %% ignore delay
	  delay   = 0 :: timeout(),     %% activation delay
	  rampup  = 0 :: timeout(),    %% analog ramp up time
	  rampdown = 0 :: timeout(),  %% analog ramp down time
	  sustain = 0 :: timeout(),   %% time to stay active
	  deact   = 0 :: timeout(),     %% deactivation delay
	  wait    = 0 :: timeout(),      %% delay between re-activation

	  feedback = false :: boolean(), %% feedback frames as input 
	  actions = []
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Config) ->
    gen_fsm:start_link(?MODULE, Config, []).

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
init({_Flags,Actions}) ->
    S0 = #state { actions = Actions },
    %% FIXME: Scan Flags and set timers etc...
    %% lets assume that state is off default for now.
    {ok, state_off, S0#state{ value = S0#state.default }}.

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
state_off({digital,Val,_}, State) when Val =/= 0 ->
    {next_state, state_on, State};
state_off(_Event, State) ->
    {next_state, state_off, State}.

state_on({digital,0,_}, State) ->
    {next_state, state_off, State};
state_on(_Event, State) ->
    {next_state, state_on, State}.

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
