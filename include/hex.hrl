
-define(HEX_DIGITAL,        16#6000).  %% channel, 0|1
-define(HEX_ANALOG,         16#6400).  %% channel, 0x0000 - 0xFFFF
-define(HEX_ENCODER,        16#6100).  %% -1,+1

-type base_pattern() :: {mask,Mask::unsigned32(),Match::unsigned32()} |
			{not_mask,Mask::unsigned32(),Match::unsigned32()} |
			{range,Low::integer(),Hight::integer()} |
			unsigned32().
			
-type pattern() :: base_pattern() | [base_pattern()].

-record(hex_signal,
	{
	  id      :: unsigned32(),   %% signal ID
	  chan    :: unsigned8(),    %% signal channel
	  type    :: unsigned16(),   %% signal type (digital/analog..)
	  value   :: unsigned32(),   %% signal value
	  source  :: term()          %% signal source identifier
	}).

-record(hex_rule,
	{
	  id    = []  :: pattern(),  %% signal id pattern
	  chan  = []  :: pattern(),  %% signal channel pattern
	  type  = []  :: pattern(),  %% signal type pattern
	  value = []  :: pattern(),  %% signal value pattern
	  output  :: [unsigned8()]
	}).
	  
