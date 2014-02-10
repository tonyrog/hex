%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex definitions
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-define(HEX_DIGITAL,        16#6000).  %% channel, 0|1
-define(HEX_ANALOG,         16#6400).  %% channel, 0x0000 - 0xFFFF
-define(HEX_ENCODER,        16#6100).  %% -1,+1
-define(HEX_RFID,           16#6200).  %% channel=type data=rfid:32

-type base_pattern() :: 
	uint32() |
	{mask,Mask::uint32(),Match::uint32()} |
	{range,Low::integer(),High::integer()} |
	{'not',base_pattern()} |
	{'and',base_pattern(),base_pattern()} |
	{'or',base_pattern(),base_pattern()}.
			
-type pattern() :: base_pattern() | [base_pattern()].
-type uint8() :: 0..16#ff.
-type uint16() :: 0..16#ffff.
-type uint32() :: 0..16#ffffffff.
-type int8()   :: -16#80..16#7f.
-type int16()  :: -16#8000..16#7fff.
-type int32()  :: -16#80000000..16#7fffffff.

-define(is_uint8(X),  (((X) band (bnot 16#ff)) == 0) ).
-define(is_uint16(X), (((X) band (bnot 16#ffff)) == 0) ).
-define(is_uint32(X), (((X) band (bnot 16#ffffffff)) == 0) ).
-define(is_int8(X), ( ( ((X) >= -16#80) andalso ((X) =< 16#7f)) )).
-define(is_int16(X), ( ( ((X) >= -16#8000) andalso ((X) =< 16#7fff)) )).
-define(is_int32(X), ( ( ((X) >= -16#80000000) andalso ((X) =< 16#7fffffff)) )).

-record(hex_signal,
	{
	  id      :: uint32(),   %% signal ID
	  chan    :: uint8(),    %% signal channel
	  type    :: uint16(),   %% signal type (digital/analog..)
	  value   :: uint32(),   %% signal value
	  source  :: term()      %% signal source identifier
	}).

%% input rules
-record(hex_rule,
	{
	  label :: atom() | integer(),
	  id    = []  :: pattern(),  %% signal id pattern
	  chan  = []  :: pattern(),  %% signal channel pattern
	  type  = []  :: pattern(),  %% signal type pattern
	  value = []  :: pattern(),  %% signal value pattern
	  output  :: [uint8()]   %% list of output channels
	}).
	  
-record(hex_event, 
	{
	  label :: atom() | integer(),
	  app :: atom(),               %% application name of plugin
	  flags :: [{atom(),term()}],  %% event application flags
	  signal :: #hex_signal{}      %% signal to send
	}).

-record(hex_output,
	{
	  label :: atom() | integer(),
	  flags = [] :: [atom() | {atom(),term()}],
	  actions = []
	}).

	  
