Specification of HomeExchange (Hex) configuration file

config() :: event() | input() | output()

#Event processing

	event() :: {event,Label::id(),{App::app(),Flags::[plugin_input_flag()]},
		        {ID::event_id(),
			     Chan::event_chan(),
				 Type::event_type(),
				 Value::value_pattern()}}

	event_id() :: can_id().
	event_channel() :: subid().
	event_type() :: digital | analog | encoder.
	app() :: atom()
	plugin_input_flag() :: atom() || {atom(),term()}
	value_pattern() :: uint32() | variable()

#Input processing

	input() :: {input,Label::id(),
	           {ID::pattern(),
			    Chan::pattern(),
				Type::pattern(),
				Value::patern(),
			    [input_flag()],[output_id()]}

    input_flag() ::
		digial |
		analog |
		encoder |
		analog_to_digital |
		springback |
		push_encoder |
		inc_encoder |
		dec_encoder |
		off_inly |
		on_only |
		{analog_min,  uint16()} |
		{analog_max,  uint16()} |
		{analog_offs, int16()} |
		{analog_scale, uint8()}

#Output processing
	output() :: {output,Label::id(),[output_flag()],[action()]}

	output_flag() ::
		{type,digital | analog | interval | puls} |
		{default, uint32()} |
		{delay, timeout()} |
	    {rampup, timeout()} |
		{rampdown, timeout()} |
		{sustain, timeout()} |
		{deact, timeout()} |
		{wait, timeout()} |
		{inhbit,timeout()} |
		{anself, boolean()} |
		{ancan, boolean()} |

	action() :: {App:app(),Flags::[plugin_output_flag()]}.
    plugin_output_flag() :: atom() | {atom(),term()}.


#Common types

	id() :: atom() | integer().
	
	can_id() :: uint11() | {cob,uint11()} | {cob,func(),ID::uint7()} |
		{xcob,func(),ID::uint25()} | {xcob,uint29()}.
	         
	func() :: nmt | sync | time_stamp |
		      pdo1\_tx | pdo1\_rx |
  		      pdo2\_tx | pdo2\_rx |
   		      pdo3\_tx | pdo3\_rx |
   		      pdo4\_tx | pdo4\_rx |
   		      sdo\_tx | sdo\_rx |
			  node_guard | lss | emergency |
			  uint4().

