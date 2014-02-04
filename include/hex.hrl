
-define(HEX_DIGITAL,        16#6000).  %% channel, 0|1
-define(HEX_ANALOG,         16#6400).  %% channel, 0x0000 - 0xFFFF
-define(HEX_ENCODER,        16#6100).  %% -1,+1

-type base_pattern() :: 
	unsigned32() |
	{mask,Mask::unsigned32(),Match::unsigned32()} |
	{range,Low::integer(),High::integer()} |
	{'not',base_pattern()} |
	{'and',base_pattern(),base_pattern()} |
	{'or',base_pattern(),base_pattern()}.
			
-type pattern() :: base_pattern() | [base_pattern()].
-type unsigned32() :: 0..16#ffffffff.
-type unsigned16() :: 0..16#ffff.
-type unsigned8() :: 0..16#ff.

-record(hex_signal,
	{
	  id      :: unsigned32(),   %% signal ID
	  chan    :: unsigned8(),    %% signal channel
	  type    :: unsigned16(),   %% signal type (digital/analog..)
	  value   :: unsigned32(),   %% signal value
	  source  :: term()          %% signal source identifier
	}).

%% input rules
-record(hex_rule,
	{
	  label :: atom() | integer(),
	  id    = []  :: pattern(),  %% signal id pattern
	  chan  = []  :: pattern(),  %% signal channel pattern
	  type  = []  :: pattern(),  %% signal type pattern
	  value = []  :: pattern(),  %% signal value pattern
	  output  :: [unsigned8()]   %% list of output channels
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

	  
