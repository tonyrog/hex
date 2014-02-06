Specification of HomeExchange (Hex) configuration file

    config() :: event() | input() | output()

# Event processing

    event() ::
       {event,
	     Label::id(),
         {App::app(),Flags::[plugin_input_flag()]},
         { ID::event_id(),
           Chan::event_chan(),
           Type::event_type(),
           Value::pattern()
         }}

	event_id() :: can_id().
	event_channel() :: subid().
	event_type() :: digital | analog | encoder.
	app() :: atom()
	plugin_input_flag() :: atom() || {atom(),term()}

# Input processing

	input() ::
      {input,
       Label::id(),
	   ID::pattern(),
       Chan::pattern(),
       Type::pattern(),
       Value::pattern(),
       [input_flag()],[output_id()]
      }

    input_flag() ::
		springback   |
		push_encoder |
	    analog_to_digital |
	    digital_to_analog |		
	    on_only  |
	    off_only |
		inc_encoder  |
		dec_encoder  |
		{encoder_ival, unsigned16()} |
	    {encoder_pause, unsigned16()} |
	    {encoder_step,  integer16()} |
		{analog_min,  unsigned16()} |
		{analog_max,  unsigned16()} |
		{analog_offs, integer16() } |
		{analog_scale, unsigned8()}

#Output processing

    output() ::
      {output,
       Label::id(),
       [output_flag()],action()
      }

	output_flag() ::
		{type,digital | analog | interval | puls} |
		{default, unsigned32()} |
		{delay, timeout()} |
	    {rampup, timeout()} |
		{rampdown, timeout()} |
		{sustain, timeout()} |
		{deact, timeout()} |
		{wait, timeout()} |
		{inhbit,timeout()} |
		{feedback, boolean()} |
		{analog_min,  unsigned16()} |
		{analog_max,  unsigned16()} |
		{analog_offs, integer16() } |
		{analog_scale, unsigned8()}		

    action() :: plugin_action() | [{pattern(),plugin_action()}].
	plugin_action() :: {App:app(),Flags::[plugin_output_flag()]}.
    plugin_output_flag() :: atom() | {atom(),term()}.

#Common types

	id() :: atom() | integer().

	base_pattern() ::
        unsigned32() |
        digital | analog | encoder |
        variable() |
		{mask,Mask::unsigned32(),Match::unsigned32()} |
		{range,Low::integer(),High::integer()} |
		{'not',base_pattern()} |
		{'and',base_pattern(),base_pattern()} |
		{'or',base_pattern(),base_pattern()}.

	variable() :: atom().
		
	pattern() :: base_pattern() | [base_pattern()].
	
	
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

#Example

Event generators

    {event, sms1, {hex_sms, [{reg_exp, "^[\\s]*(O|o)ff[\\s]*$"}]},
	   {{xcobid, pdo1_tx, 16#20001}, 1, digital, 0}}.

    {event, sms2, {hex_sms, [{reg_exp, "^[\\s]*(O|o)n[\\s]*$"}]},
	   {{xcobid, pdo1_tx, 16#20001}, 1, digital, 1} }.

    {event, gpio1, {hex_gpio, [{board, piface},{pin, 0},
                           {interrupt, falling},{polarity, true}]},
       {{xcobid, pdo1_tx, 16#20001}, 100, digital, 0} }.

	{event, gpio2, {hex_gpio, [{board, cpu},{pin_reg,0},{pin, 17},
                               {interrupt, both}]},
	   {{xcobid, pdo1_tx, 16#20001}, 117, digital, value} }.

	{event, rf1, {hex_tellstick, [{protocol,arctech},{model,codeswitch},
	                              {data,16#E0D}]},
	   {{xcobid, pdo1_tx, 16#20001}, 14, digital, 0} }.

	{event, rf2, {hex_tellstick, [{protocol,arctech},
	                              {model,selflearning},{data,16#1050}]},
       {{xcobid, pdo1_tx, 16#20001}, 14, digital, 1} }.

Input rules

	{input,in1, {xcobid, pdo1_tx, 16#20001}, 20, [], [], [], [1]}

	{input,in2, {xcobid, pdo1_tx, 16#20001}, 21, [], [], [], [2]}.

    {input,in3, {xcobid, pdo1_tx, 16#20001}, 12, [], [], [], [3]}

    {input,in4, {xcobid, pdo1_tx, 16#20001},  4, [], [], [], [4]}.

    {input,in5, {xcobid, pdo1_tx, 16#20001},  5, digital, 1, [], [2,3,4]}.

Output configuration and actions

	{output, 1, [digital],
		{hex_gpio, [{board, cpu}, {pin_reg,0}, {pin, 17}]} }.

	{output, 2, [digital],
		{hex_gpio, [{board, piface}, {pin_reg,0}, {pin, 1}]} }.

    {output, 3, [digital,{inhibit,10000}],
		[ {1, {hex_email,
		      [{account,foo},
               {subject, "rfzone"}, {sender,"from@mail.com"},
               {recipients,["x@mail.com","y.mail.com"]},
               {from, "Joe Smith"}, {to,   "Jane Smith"},
               {date, true}, {message_id, true},
               {body, "Alert, door is open!\n"}]}},
	      {0, {hex_email,
		      [{account,foo},
               {subject, "rfzone"}, {sender,"from@mail.com"},
               {recipients,["x@mail.com","y.mail.com"]},
               {from, "Joe Smith"}, {to,   "Jane Smith"},
               {date, true}, {message_id, true},
               {body, "Alert, door is closed!\n"}]}}
		   ]}.

	{output, 4, [digital,analog,{analog_min,0},{analog_max,10},{style, smooth}],
		{hex_tellstick,
			[{protocol,ikea}, {unit,1}, {channel,1}]} }.

The account info for email is located in sys.config as
plugin app hex\_email data.

    {hex_email, foo, [
      {relay, "smtp.mail.com"},
      {username, "user"},
      {password, "password"},
      {port,2525}]}.
