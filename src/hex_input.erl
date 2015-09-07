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
%%%    HEX input processor
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_input).

-behaviour(gen_fsm).

-include("../include/hex.hrl").
%% API
-export([start_link/1]).
-export([validate_flags/1]).
-export([event_spec/0]).


%% gen_fsm callbacks
-export([init/1, s_neutral/2, s_push/2,
	 handle_event/3, handle_sync_event/4,
	 handle_info/3, terminate/3, code_change/4]).

-define(S_NEUTRAL,    s_neutral).
-define(S_PUSH,       s_push).

-define(SERVER, ?MODULE).

-define(UPPER_LIMIT_EXCEEDED,                16#01).
-define(BELOW_LOWER_LIMIT,                   16#02).
-define(CHANGED_BY_MORE_THAN_DELTA,          16#04).
-define(CHANGED_BY_MORE_THAN_NEGATIVE_DELTA, 16#08).
-define(CHANGED_BY_MORE_THAN_POSITIVE_DELTA, 16#10).
-define(LIMIT_BITS, 16#03).
-define(DELTA_BITS, 16#1C).

-record(opt,
	{
	  digital    = true   :: boolean(),
	  analog     = true   :: boolean(),
 	  rfid       = true   :: boolean(),
 	  encoder    = true   :: boolean(),
	  on_only    = false  :: boolean(),
	  off_only   = false  :: boolean(),
	  springback = false  :: boolean(),
	  invert     = false  :: boolean(),
	  analog_to_digital = false :: boolean(),
	  digital_to_analog = false :: boolean(),
	  %% encoder config
	  push_encoder = false :: boolean(),
	  inc_encoder = false  :: boolean(),
	  dec_encoder = false  :: boolean(),
	  encoder_ival  = 250 :: uint32(),
	  encoder_pause = 3000 :: uint32(),
	  encoder_step = 1 :: int32(),
	  %% analog config
	  analog_trigger = ?CHANGED_BY_MORE_THAN_DELTA :: uint8(),
	  analog_delta = 0 :: uint32(),
	  analog_negative_delta = 1 :: uint32(),
	  analog_positive_delta = 1 :: uint32(),
	  analog_max_frequency = 0  :: float(),
	  inhibit_us = 0 :: uint32(),  %% derived from max_frequency
	  analog_upper_limit = 16#ffff :: int32(),
	  analog_lower_limit = 16#0000 :: int32(),
	  analog_min = 0               :: int32(),
	  analog_max = 16#ffff         :: int32(),
	  analog_offs = 0              :: int32(),
	  analog_scale = 1.0           :: float(),
	  %% rfid config
	  rfid_match = 0 :: uint32(),
	  rfid_mask  = 0 :: uint32(),
	  rfid_match_to_digital :: boolean(),
	  outputs = [] :: [{atom(),uint8()}]
	}).

-record(s,
	{
	  id           :: atom() | integer(),
	  value = 0    :: uint32(),             %% last (digital) value
	  an_value=0   :: uint32(),             %% last scaled analog value
	  an_mask = 0  :: uint32(),             %% analog latch_mask
	  an_inhibit = 0  :: uint32(),          %% abs micro seconds
	  src          :: term(),               %% last source
	  timestamp    :: erlang:timestamp(),   %% last time
	  timer        :: reference() | undefined,
	  estep = 0    :: integer(),            %% push encoder value
	  config       :: #opt{}
	}).

%%%===================================================================
%%% API
%%%===================================================================

%% verify input_flags
validate_flags(Flags) ->
    hex:validate_flags(Flags, event_spec()).
%%    case set_options(Flags, #opt {}) of
%%	{ok,_} -> ok;
%%	Error -> Error
%%    end.

event_spec() ->
    [
     {leaf, active, [{type,boolean,[]},
		      {description, "Input active state.",[]},
		      {default, true, []}]},
     {leaf, digital, [{type,boolean,[]},
		      {description, "Allow digital input signals.",[]},
		      {default, true, []}]},
     {leaf, analog, [{type,boolean,[]},
		     {description, "Allow analog input signals.",[]},
		     {default, true, []}]},
     {leaf, encoder, [{type,boolean,[]},
		     {description, "Allow encoder input signals.",[]},
		      {default, true, []}]},
     {leaf, rifd, [{type,boolean,[]},
		   {description, "Allow rfid input signals.",[]},
		   {default, true, []}]},

     {leaf, analog_to_digital,
      [{type,boolean,[]},
       {description, "Convert analog to digital signals.",[]},
       {default, false, []}]},

     {leaf, digital_to_analog,
      [{type,boolean,[]},
       {description, "Convert digital to analog signals.",[]},
       {default, false, []}]},

     {leaf, on_only,
      [{type,boolean,[]},
       {description, "Accept digital on signals only.",[]},
       {default, false, []}]},

     {leaf, off_only,
      [{type,boolean,[]},
       {description, "Accept digital off signals only.",[]},
       {default, false, []}]},

     {leaf, springback,
      [{type,boolean,[]},
       {description, "Input is from a 'springback' button.",[]},
       {default, false, []}]},

     {leaf, invert,
      [{type,boolean,[]},
       {description, "Digital input is inverted.",[]},
       {default, false, []}]},

     {leaf, push_encoder,
      [{type,boolean,[]},
       {description, "Activate push encoder functionallity.",[]},
       {default, false, []}]},

     {leaf, inc_encoder,
      [{type,boolean,[]},
       {description, "Push encoder that only increaments.",[]},
       {default, false, []}]},

     {leaf, dec_encoder,
      [{type,boolean,[]},
       {description, "Push encoder that only decrements.",[]},
       {default, false, []}]},

     {leaf, encoder_ival,
      [{type, uint32, []},
       {description, "Push encoder update interval.", []},
       {default, 250, []}]},

     {leaf, encoder_pause,
      [{type, uint32, []},
       {description, "Push encoder direction switch timeout.", []},
       {default, 3000, []}]},

     {leaf, encoder_step,
      [{type, uint32, []},
       {description, "Push encoder update step.", []},
       {default, 1, []}]},

     {leaf, analog_delta,
      [{type, uint32, []},
       {description, "Analog delta value.", []},
       {default, 0, []}]},

     {leaf, analog_trigger,
      [{type, bits,
	[{bit, 'upper-limit-exceeded', [{position,0,[]}]},
	 {bit, 'below_lower_limit', [{position,1,[]}]},
	 {bit, 'changed-by-more-than-delta',[{position,2,[]}]},
	 {bit, 'changed-by-more-than-negative-delta',[{position,3,[]}]},
	 {bit, 'changed-by-more-than-positive-delta',[{position,4,[]}]}
	]},
       {default, ?CHANGED_BY_MORE_THAN_DELTA, []}
      ]},

     {leaf, analog_negative_delta,
      [{type, uint32, []},
       {description, "Analog negative delta value.", []},
       {default, 1, []}]},

     {leaf, analog_positive_delta,
      [{type, uint32, []},
       {description, "Analog positive delta value.", []},
       {default, 1, []}]},

     {leaf, analog_max_frequency,
      [{type, decimal64, [{'fraction-digits', 6, []}]},
       {description, "Analog max output frequency.", []},
       {default, 0, []}]},

     {leaf, analog_lower_limit,
      [{type, int32, []},
       {description, "Analog lower limit.", []},
       {default, 0, []}]},

     {leaf, analog_upper_limit,
      [{type, int32, []},
       {description, "Analog upper limit.", []},
       {default, 16#ffff, []}]},

     {leaf, analog_min,
      [{type, int32, []},
       {description, "Analog min value.", []},
       {default, 0, []}]},

     {leaf, analog_max,
      [{type, int32, []},
       {description, "Analog max value.", []},
       {default, 16#ffff, []}]},

     {leaf, analog_offs,
      [{type, int32, []},
       {description, "Analog offset value.", []},
       {default, 0, []}]},

     {leaf, analog_scale,
      [{type, decimal64, [{'fraction-digits',6,[]}]},
       {description, "Analog offset value.", []},
       {default, 1.0, []}]},

     {leaf, rfid_match,
      [{type, uint32, []},
       {default, 0, []}]},

     {leaf, rfid_mask,
      [{type, uint32, []},
       {default, 0, []}]},

     {leaf, rfid_match_to_digital,
      [{type, boolean, []},
       {default, false, []}]},

     {list, output,
      [{key,  channel, []},
       {leaf, channel, [{type, uint8, [{range,[{1,254}],[]}]}]},
       {leaf, target, [{type, string, []},
		       {default, value, []}]}
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
start_link(Flags) ->
    gen_fsm:start_link(?MODULE, Flags, []).

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
init(Flags) ->
    case set_options(Flags, #opt {}) of
	{ok, Config} ->
	    {ok, ?S_NEUTRAL, #s { timestamp = timestamp_us(),
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
s_neutral({rfid,Value,Src}, S) ->
    rfid_input(Value, Src, ?S_NEUTRAL, S);
%% digital/analog/encoder MUST be translated by dispatcher!
s_neutral({Type,Value,Src}, S) when ?is_uint16(Type) ->
    output(Type,Value,Src,?S_NEUTRAL,S);
s_neutral(pulse_done, S) ->
    lager:debug("s_neutral:pulse_done", []),
    {next_state, ?S_NEUTRAL, S#s { estep = 0 }};
s_neutral(_Value, S) ->
    lager:debug("garbage ~w seen in state s_neutral", [_Value]),
    {next_state, ?S_NEUTRAL, S}.


%% state while push_encoder and button is not released
s_push(pulse, S) ->
    T = gen_fsm:send_event_after((S#s.config)#opt.encoder_ival, pulse),
    encoder_input(S#s.estep, S#s.src, ?S_PUSH,S#s { timer = T });
s_push(pulse_done, S) ->
    lager:debug("s_push:pulse_done (ignore)", []),
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

analog_input(IValue, Src, State, S) ->
    Opt = S#s.config,
    Value = trunc(IValue*Opt#opt.analog_scale + Opt#opt.analog_offs),
    lager:debug("analog_input ~w: value=~w", [S#s.id,Value]),
    %% transform value
    if Opt#opt.analog_to_digital ->
	    if Value >= Opt#opt.analog_max ->
		    digital_input(1, Src, State, S);
	       Value =< Opt#opt.analog_min ->
		    digital_input(0, Src, State, S);
	       true ->
		    {next_state, State, S}
	    end;
       Value >= Opt#opt.analog_max ->
	    output(analog,Opt#opt.analog_max,Src,State,S);
       Value =< Opt#opt.analog_min ->
	    output(analog,Opt#opt.analog_min,Src,State,S);
       true ->
	    output(analog,Value,Src,State,S)
    end.

rfid_input(Value,Src,State,S) ->
    Opt = S#s.config,
    if Value band Opt#opt.rfid_mask =:= Opt#opt.rfid_match ->
	    if Opt#opt.rfid_match_to_digital ->
		    digital_input(1,Src,State,S);
	       true ->
		    output(rfid,Value,Src,State,S)
	    end;
       true ->
	    {next_state, State, S}
    end.

digital_input(Value0,Src,State,S) ->
    Opt = S#s.config,
    Value = if Opt#opt.invert -> 1-Value0;
	       true -> Value0
	    end,
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

output(analog,Value,Src,State,S) ->
    Opt = S#s.config,
    Now = timestamp_us(), %% fixme: read this somewhere (pass in signal?)
    Delta = Value - S#s.an_value,
    %% calculate trigger bits
    Mask =
	if Value > Opt#opt.analog_upper_limit -> ?UPPER_LIMIT_EXCEEDED;
	   true -> 0
	end bor
	if Value =< Opt#opt.analog_lower_limit -> ?BELOW_LOWER_LIMIT;
	   true -> 0
	end bor
	if Delta > 0 ->
		if Delta > Opt#opt.analog_delta ->
			?CHANGED_BY_MORE_THAN_DELTA;
		   true -> 0
		end bor
		    if
			Delta > Opt#opt.analog_positive_delta ->
			    ?CHANGED_BY_MORE_THAN_POSITIVE_DELTA;
			true -> 0
		    end;
	   Delta < 0 ->
		if Delta < -Opt#opt.analog_delta ->
			?CHANGED_BY_MORE_THAN_DELTA;
		   Delta < -Opt#opt.analog_negative_delta ->
			?CHANGED_BY_MORE_THAN_NEGATIVE_DELTA;
		   true ->
			0
		end;
	   true ->
		0
	end,
    %% Delta changes always trigger, but level changes only trigger
    %% after the levels have been reset
    Mask1 = Mask band Opt#opt.analog_trigger,

    Latch = (Mask1 band ?DELTA_BITS =/= 0)
	orelse
	  ((Mask1 band ?UPPER_LIMIT_EXCEEDED =/= 0)
	   andalso
	     (S#s.an_mask band ?UPPER_LIMIT_EXCEEDED =:= 0))
	orelse
	  ((Mask1 band ?BELOW_LOWER_LIMIT =/= 0)
	   andalso
	     (S#s.an_mask band ?BELOW_LOWER_LIMIT =:= 0)),

    if Latch, Now > S#s.an_inhibit ->
	    lager:debug("Output: ~w, trigger=~p", [Value,get_trigger(Mask1)]),
	    send_output(analog,Value,Src,S),
	    T1 = Now+Opt#opt.inhibit_us,
	    {next_state, State, S#s { an_value = Value,
				      src=Src,
				      an_mask = Mask,
				      timestamp = Now,
				      an_inhibit = T1 }};
       true ->
	    {next_state, State, S#s { src=Src,
				      timestamp = Now }}
    end;
output(Type,Value,Src,State,S) ->
    send_output(Type,Value,Src,S),
    {next_state, State, S#s { value = Value, src=Src }}.

send_output(Type,Value,Src,S) ->
    Output = { Type, Value, Src },
    lists:foreach(fun({Target,Channel}) ->
			  hex_server:output(Channel,Target,Output)
		  end, (S#s.config)#opt.outputs).

set_options([{Option,Value} | Options], Opt) ->
    try set_option(Option, Value, Opt) of
	Opt1 -> set_options(Options, Opt1)
    catch
	error:_ ->
	    {error, {badarg, {Option, Value}}}
    end;
set_options([], Opt) ->
    {ok, Opt}.

set_option(K, V, Opt) ->
    case K of
	digital when is_boolean(V) -> Opt#opt { digital = V };
	analog  when is_boolean(V) -> Opt#opt { analog = V };
	encoder when is_boolean(V) -> Opt#opt { encoder = V };
	rfid  when is_boolean(V) -> Opt#opt { rfid = V };
	analog_to_digital when is_boolean(V) ->
	    Opt#opt { analog_to_digital = V };
	digital_to_analog when is_boolean(V) ->
	    Opt#opt { digital_to_analog = V };
	on_only when is_boolean(V) ->  Opt#opt { on_only = V };
	off_only when is_boolean(V) -> Opt#opt { off_only = V };
	springback when is_boolean(V) -> Opt#opt { springback = V };
	invert     when is_boolean(V) -> Opt#opt { invert = V };
	push_encoder when is_boolean(V) -> Opt#opt { push_encoder = V };
	inc_encoder when is_boolean(V) -> Opt#opt { inc_encoder = V };
	dec_encoder when is_boolean(V) -> Opt#opt { dec_encoder = V };
	encoder_ival when ?is_uint32(V) -> Opt#opt { encoder_ival = V };
	encoder_pause when ?is_uint32(V) -> Opt#opt { encoder_pause = V };
	encoder_step when ?is_int32(V) -> Opt#opt { encoder_step = V };
	analog_delta when ?is_uint32(V) -> Opt#opt { analog_delta = V };
	analog_trigger when is_atom(V); is_list(V) ->
	    Opt#opt { analog_trigger = make_trigger(V) };
	analog_negative_delta when ?is_uint32(V) ->
	    Opt#opt { analog_negative_delta = V };
	analog_positive_delta when ?is_uint32(V) ->
	    Opt#opt { analog_positive_delta = V };
	analog_max_frequency when is_number(V) ->
	    T = trunc(1000000.0/V),
	    Opt#opt { analog_max_frequency = V, inhibit_us = T };
	analog_upper_limit when ?is_int32(V) ->
	    Opt#opt { analog_upper_limit = V };
	analog_lower_limit when ?is_int32(V) ->
	    Opt#opt { analog_lower_limit = V };
	analog_min when ?is_int32(V) -> Opt#opt { analog_min = V };
	analog_max when ?is_int32(V) -> Opt#opt { analog_max = V };
	analog_offs when ?is_int32(V) -> Opt#opt { analog_offs = V };
	analog_scale when is_float(V) ->  Opt#opt { analog_scale = V };
	%% rfid
	rfid_match when ?is_uint32(V) -> Opt#opt { rfid_match = V };
	rfid_mask  when ?is_uint32(V) -> Opt#opt { rfid_mask  = V };
	rfid_match_to_digital when is_boolean(V) ->
	    Opt#opt { rfid_match_to_digital = V };
	%% output list
	output when is_list(V) ->
	    Channel = proplists:get_value(channel, V),
	    Target  = proplists:get_value(target, V, in),
	    if is_integer(Channel), Channel >= 1, Channel =< 254,
	       is_atom(Target) ->
		    Opt#opt { outputs = [{Target,Channel}|Opt#opt.outputs]}
	    end
    end.

make_trigger([H|T]) ->
    make_trigger(H) bor make_trigger(T);
make_trigger([]) -> 0;
make_trigger('upper-limit-exceeded') ->   ?UPPER_LIMIT_EXCEEDED;
make_trigger('below_lower_limit') ->    ?BELOW_LOWER_LIMIT;
make_trigger('changed-by-more-than-delta') ->    ?CHANGED_BY_MORE_THAN_DELTA;
make_trigger('changed-by-more-than-negative-delta') ->
    ?CHANGED_BY_MORE_THAN_NEGATIVE_DELTA;
make_trigger('changed-by-more-than-positive-delta') ->
    ?CHANGED_BY_MORE_THAN_POSITIVE_DELTA.

get_trigger(Mask) ->
    select_bits(Mask, ['upper-limit-exceeded',
		       'below-lower-limit',
		       'changed-by-more-than-delta',
		       'changed-by-more-than-negative-delta',
		       'changed-by-more-than-positive-delta']).
select_bits(0, _) ->
    [];
select_bits(Mask, [Name|Names]) when Mask band 1 =:= 1 ->
    [Name | select_bits(Mask bsr 1, Names)];
select_bits(Mask, [_|Names]) ->
    select_bits(Mask bsr 1, Names).

timestamp_us() ->
    timestamp_us(os:timestamp()).

timestamp_us({M,S,U}) ->
    ((M*1000000+S)*1000000 + U).
