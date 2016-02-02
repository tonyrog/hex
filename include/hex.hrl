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
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Hex definitions
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-define(HEX_DIGITAL,        16#6000).  %% channel, 0|1
-define(HEX_ANALOG,         16#6400).  %% channel, 0x0000 - 0xFFFF
-define(HEX_ENCODER,        16#6100).  %% -1,+1
-define(HEX_RFID,           16#6200).  %% channel=type data=rfid:32
-define(HEX_BATTERY,        16#6201).  %% sub=bank, data=volt:16, amp:16
-define(HEX_LOAD,           16#6202).  %% data=amp:16  amp=A*100

-define(HEX_POWER_ON,       16#2800).  %% Power ON
-define(HEX_POWER_OFF,      16#2801).  %% Sent by unit before powerdown
-define(HEX_WAKEUP,         16#2802).  %% Sent in wakeup signal
-define(HEX_ALARM,          16#2803).  %% Alarm code notification

-define(HEX_OUTPUT_ADD,     16#2804).  %% Add output interface
-define(HEX_OUTPUT_DEL,     16#2805).  %% Delete output interface
-define(HEX_OUTPUT_ACTIVE,  16#2806).  %% Active/Deactivate signal
-define(HEX_OUTPUT_VALUE,   16#280D).  %% Current value update

-define(HEX_ALARM_CNFRM_ACK,16#2809).   %% Alarm ack code notification
-define(HEX_ALARM_CNFRM,    16#280E).   %% Confirm larm condition (from client)

-define(HEX_OUTPUT_STATE,   16#2814).   %% output-active/output-value
-define(HEX_OUTPUT_ALARM,   16#2815).   %% send output-alarm on match

-define(HEX_COBID_EXT,      16#20000000).
-define(HEX_XNODE_ID_MASK,  16#01FFFFFF).  %% 25 bit node id

-type base_pattern() ::
	uint32() |
	{mask,Mask::uint32(),Match::uint32()} |
	{range,Low::integer(),High::integer()} |
	{'not',base_pattern()} |
	{'and',base_pattern(),base_pattern()} |
	{'or',base_pattern(),base_pattern()}.

-type pattern() :: base_pattern() | [base_pattern()].

-type bits_size() :: integer().
-type bits_name() :: '_' | atom() | integer() | float().
-type bits_pattern() :: { bits_size(), bits_name() }.
-type bin_pattern() :: [ bits_pattern() ].

-type uint1() :: 0..1.
-type uint8() :: 0..16#ff.
-type uint16() :: 0..16#ffff.
-type uint32() :: 0..16#ffffffff.
-type uint64() :: 0..16#ffffffffffffffff.
-type int8()   :: -16#80..16#7f.
-type int16()  :: -16#8000..16#7fff.
-type int32()  :: -16#80000000..16#7fffffff.
-type int64()  :: -16#8000000000000000..16#7fffffffffffffff.

-define(is_uint1(X),  (((X) band (bnot 16#1)) == 0) ).
-define(is_uint4(X),  (((X) band (bnot 16#f)) == 0) ).
-define(is_uint7(X),  (((X) band (bnot 16#7f)) == 0) ).
-define(is_uint8(X),  (((X) band (bnot 16#ff)) == 0) ).
-define(is_uint11(X),  (((X) band (bnot 16#7ff)) == 0) ).
-define(is_uint16(X), (((X) band (bnot 16#ffff)) == 0) ).
-define(is_uint24(X), (((X) band (bnot 16#ffffff)) == 0) ).
-define(is_uint25(X), (((X) band (bnot 16#1ffffff)) == 0) ).
-define(is_uint29(X), (((X) band (bnot 16#1fffffff)) == 0) ).
-define(is_uint32(X), (((X) band (bnot 16#ffffffff)) == 0) ).
-define(is_uint64(X), (((X) band (bnot 16#ffffffffffffffff)) == 0) ).
-define(is_int8(X), ( ( ((X) >= -16#80) andalso ((X) =< 16#7f)) )).
-define(is_int16(X), ( ( ((X) >= -16#8000) andalso ((X) =< 16#7fff)) )).
-define(is_int32(X), ( ( ((X) >= -16#80000000) andalso ((X) =< 16#7fffffff)) )).
-define(is_int64(X), ( ( ((X) >= -16#8000000000000000)
			 andalso ((X) =< 16#7fffffffffffffff)) )).
-define(is_timeout(T), ?is_uint32(T)).

-record(hex_signal,
	{
	  id      :: uint32(),   %% signal ID
	  chan    :: uint8(),    %% signal channel
	  type    :: uint16(),   %% signal type (digital/analog..)
	  value   :: uint32(),   %% signal value
	  source  :: term()      %% signal source identifier
	}).

-record(hex_pattern,
	{
	  id    = []  :: pattern(),  %% signal id pattern
	  chan  = []  :: pattern(),  %% signal channel pattern
	  type  = []  :: pattern(),  %% signal type pattern
	  value = []  :: pattern()   %% signal value pattern
	}).

-record(hex_bin_pattern,
	{
	  id    = [] :: pattern(),   %% signal id pattern
	  bin   = [] :: bin_pattern()
	}).


%% input rules
-record(hex_input,
	{
	  label :: atom() | integer(),  %% rule id
	  signal :: #hex_pattern{} | #hex_bin_pattern{}, %% input match pattern
	  flags :: [{atom(),term()}]    %% input state flags
	}).

-record(hex_event,
	{
	  label     :: atom() | integer(),
	  flags = [] :: [{atom(),term()}],
	  app       :: atom(),              %% application name of plugin
	  app_flags :: [{atom(),term()}],   %% event application flags
	  signal    :: #hex_signal{}        %% signal to send/match
	}).

-record(hex_output,
	{
	  label :: atom() | integer(),
	  flags = [] :: [{atom(),term()}],
	  actions = []
	}).

-record(hex_transmit,
	{
	  label :: atom() | integer(),
	  app   :: atom(),      %% signal transmittion app
	  flags = [] :: [{atom(),term()}],
	  signal :: #hex_pattern{}     %% signal to match
	}).

-type hex_config() :: #hex_transmit{} |
		      #hex_event{} |
		      #hex_input{} |
		      #hex_output{} .
