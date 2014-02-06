%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    HEX input processor - keep minimal state 
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_input).

-behaviour(gen_fsm).

-include("../include/hex.hrl").
%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, s_neutral/2, s_push/2, 
	 handle_event/3, handle_sync_event/4, 
	 handle_info/3, terminate/3, code_change/4]).

-define(S_NEUTRAL,    s_neutral).
-define(S_PUSH,       s_push).

-define(SERVER, ?MODULE).

-record(opt,
	{
	  on_only    = false  :: boolean(),
	  off_only   = false  :: boolean(),
	  springback = false  :: boolean(),
	  push_encoder = false :: boolean(),
	  inc_encoder = false  :: boolean(),
	  dec_encoder = false  :: boolean(),
	  analog_to_digital = false :: boolean(),
	  digital_to_analog = false :: boolean(),
	  encoder_ival  = 250 :: unsigned16(),
	  encoder_pause = 3000 :: unsigned16(),
	  encoder_step = 1 :: integer16(),
	  analog_min = 0 :: unsigned16(),
	  analog_max = 16#ffff :: unsigned16(),
	  analog_offs = 0  :: integer16(),
	  %% stored as fixpoint 8.8, input is integer or float!
	  analog_scale = 16#0100 :: unsigned16(),
	  outputs = [] :: [unsigned8()]
	}).

-record(s,
	{
	  id           :: atom() | integer(),
	  value = 0    :: unsigned32(),         %% last value
	  src          :: term(),               %% last source
	  time         :: erlang:timestamp(),   %% last time
	  timer        :: reference() | undefined,
	  estep = 0    :: integer(),             %% push encoder value
	  config       :: #opt{}
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
start_link(Options) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, Options, []).

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
init(Options) ->
    case set_options(Options, #opt {}) of
	{ok, Config} ->
	    {ok, ?S_NEUTRAL, #s { time = os:timestamp(),
				  config = Config }};
	{error, Reason} ->
	    {stop, Reason}
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

s_neutral({digital,Value,Src}, S) ->
    digital_input(Value, Src, ?S_NEUTRAL, S);
s_neutral({analog,Value,Src}, S) ->
    analog_input(Value, Src, ?S_NEUTRAL, S);
s_neutral({encoder,Value,Src}, S) ->
    encoder_input(Value, Src, ?S_NEUTRAL, S);
%% digital/analog/encoder MUST be translated by dispatcher!
s_neutral({Type,Value,Src}, S) when ?is_unsigned16(Type) ->
    output(Type,Value,Src,?S_NEUTRAL,S);
s_neutral(pulse_done, S) ->
    io:format("s_neutral:pulse_done\n", []),
    {next_state, ?S_NEUTRAL, S#s { estep = 0 }}.


%% state while push_encoder and button is not released
s_push(pulse, S) ->
    T = gen_fsm:send_event_after((S#s.config)#opt.encoder_ival, pulse),
    encoder_input(S#s.estep, S#s.src, ?S_PUSH,S#s { timer = T });
s_push(pulse_done, S) ->
    io:format("s_push:pulse_done (ignore)\n", []),
    %% ignored since we probably changed direction
    {next_state, ?S_PUSH, S};
s_push({digital,0,_Src}, S) ->
    %% fixme handle all digital=0 cases
    gen_fsm:cancel_timer(S#s.timer),
    receive pulse -> ok after 0 -> ok end, %% flush pulse
    T = gen_fsm:send_event_after((S#s.config)#opt.encoder_pause, pulse_done),
    {next_state, ?S_NEUTRAL, S#s { timer = T}};
s_push(_, S) ->
    {next_state, ?S_PUSH, S}.



analog_input(Value, Src, State, S) ->
    Opt = S#s.config,
    %% transform value
    Value1 = ((Value*Opt#opt.analog_scale) bsr 8) + Opt#opt.analog_offs,
    if Opt#opt.analog_to_digital ->
	    if Value1 >= Opt#opt.analog_max ->
		    digital_input(1, Src, State, S);
	       Value1 =< Opt#opt.analog_min ->
		    digital_input(0, Src, State, S);
	       true ->
		    {next_state, State, S}
	    end;
       Value1 >= Opt#opt.analog_max ->
	    output(analog,Opt#opt.analog_max,Src,State,S);
       Value1 =< Opt#opt.analog_min ->
	    output(analog,Opt#opt.analog_min,Src,State,S);
       true ->
	    output(analog,Value1,Src,State,S)
    end.    

digital_input(Value,Src,State,S) ->
    Opt = S#s.config,
    if 
	Opt#opt.digital_to_analog ->
	    if Value =:= 0 ->
		    output(analog,Opt#opt.analog_min,Src,State,S);
	       true ->
		    output(analog,Opt#opt.analog_max,Src,State,S)
	    end;
	Opt#opt.push_encoder, Value =/= 0 ->
	    case S#s.estep of
		0 ->
		    Value1 = 1-S#s.value,
		    if Value1 =:= 0 ->
			    output(digital, Value1, Src, ?S_NEUTRAL, S);
		       true ->
			    T = gen_fsm:send_event_after(Opt#opt.encoder_ival,
							 pulse),
			    S1 = S#s { timer=T, estep=Opt#opt.encoder_step },
			    output(digital,1,Src,?S_PUSH, S1)
		    end;
		Estep ->
		    T = gen_fsm:send_event_after(Opt#opt.encoder_ival, pulse),
		    Estep1 = -Estep,
		    S1 = S#s { timer = T, estep = Estep1 },
		    output(encoder,Estep1,Src,?S_PUSH, S1)
	    end;
	Opt#opt.springback, Value =:= 0 ->
	    {next_state, State, S};
	Opt#opt.springback ->
	    output(digital, 1-S#s.value,Src,State,S);
	true ->
	    output(digital,Value,Src,State,S)
    end.

encoder_input(Value, Src, State, S) ->
    output(encoder,Value,Src,State,S).

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

output(digital,0,_Src,State,S) when (S#s.config)#opt.on_only ->
    {next_state, State, S#s { value = 0 }};  %% ignore value? test
output(digital,V,_Src,State,S) when V =/= 0, (S#s.config)#opt.off_only ->
    {next_state, State, S#s { value = V }};  %% ignore value? test

output(encoder,V,_Src,State,S) when (S#s.config)#opt.inc_encoder, V < 0 ->
    {next_state, State, S};
output(encoder,V,_Src,State,S) when (S#s.config)#opt.dec_encoder, V > 0 ->
    {next_state, State, S};
output(encoder,Value,Src,State,S) ->
    send_output(encoder,Value,Src,S), %% do not set value field!
    {next_state, State, S#s { src=Src }};
output(Type,Value,Src,State,S) ->
    send_output(Type,Value,Src,S),
    {next_state, State, S#s { value = Value, src=Src }}.

send_output(Type,Value,Src,S) ->
    Output = { Type, Value, Src },
    lists:foreach(fun(Out) -> hex_server:output(Out, Output) end,
		  (S#s.config)#opt.outputs).


set_options([{Option,Value} | Options], S) ->
    try set_option(Option, Value, S) of
	S1 -> set_options(Options, S1)
    catch
	error:_ ->
	    {error, {badarg, {Option, Value}}}
    end;
set_options([Option | Options], S) ->
    try set_option(Option, true, S) of
	S1 -> set_options(Options, S1)
    catch
	error:_ ->
	    {error, {badarg,Option}}
    end;
set_options([], S) ->
    {ok, S}.

set_option(K, V, Opt) ->
    case K of
	analog_to_digital when is_boolean(V) ->
	    Opt#opt { analog_to_digital = V };
	digital_to_analog when is_boolean(V) ->
	    Opt#opt { digital_to_analog = V };
	on_only when is_boolean(V) ->  Opt#opt { on_only = V };
	off_only when is_boolean(V) -> Opt#opt { off_only = V };
	springback when is_boolean(V) -> Opt#opt { springback = V };
	push_encoder when is_boolean(V) -> Opt#opt { push_encoder = V };
	inc_encoder when is_boolean(V) -> Opt#opt { inc_encoder = V };
	dec_encoder when is_boolean(V) -> Opt#opt { dec_encoder = V };
	encoder_ival when ?is_unsigned16(V) -> Opt#opt { encoder_ival = V };
	encoder_pause when ?is_unsigned16(V) -> Opt#opt { encoder_pause = V };
	encoder_step when ?is_integer16(V) -> Opt#opt { encoder_step = V };
	analog_min when ?is_unsigned16(V) -> Opt#opt { analog_min = V };
	analog_max when ?is_unsigned16(V) -> Opt#opt { analog_max = V };
	analog_offs when ?is_integer16(V) -> Opt#opt { analog_offs = V };
	analog_scale when is_integer(V), V >= 0, V =< 255 ->
	    Opt#opt { analog_scale = (V bsl 8) };
	analog_scale when is_float(V), V >= 0.0, V =< 255.0 ->
	    Opt#opt { analog_scale = min(trunc(V*256), 16#ffff) };
	output when is_list(V) ->
	    [ true = ((I >= 1) andalso (I =< 254)) || I <- V],
	    Opt#opt { outputs = V }
    end.
