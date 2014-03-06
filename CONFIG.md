Specification of HomeExchange (Hex) configuration file

    config() :: event() | input() | output() | transmit()

# Event processing

    event() ::
       {event,
	     Label::id(),
         {App::app(),Flags::[plugin_input_flag()]},
         { ID::event_id(), Chan::event_chan(), Type::event_type(),
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
	   Pattern::event_pattern(),
       [input_flag()],[out_id()|{out_channel(),out_id}]
      }

	out_channel() :: value|inhibit|delay|rampup|sustain|rampdown|
                     deact|wait|repeat

    input_flag() ::
		springback   |
		push_encoder |
	    analog_to_digital |
	    digital_to_analog |
		invert |
	    on_only  |
	    off_only |
		inc_encoder |
		dec_encoder |
		{encoder_ival,  uint32()} |
	    {encoder_pause, uint32()} |
	    {encoder_step,  int32()} |
		{analog_trigger,
			['all' | 'upper-limit-exceeded' |
			 'below_lower_limit' |
			 'changed-by-more-than-delta' |
			 'changed-by-more-than-negative-delta' |
		     'changed-by-more-than-positive-delta']},
	    {analog_delta,  uint32()} |
		{analog_negative_delta, uint32()} |
		{analog_positive_delta, uint32()} |		
		{analog_max_frequency, decimal64()}
		{analog_min,  int32()} |
		{analog_max,  int32()} |
		{analog_offs, int32() } |
		{analog_scale, int32()}

#Output processing

    output() ::
      {output,
       Label::out_id(),
       [output_flag()],
	   action()
      }

	output_flag() ::
		default     :: unsigned32()  default value
	    inhbit      :: timeout()
		delay       :: timeout()
	    rampup      :: timeout()
		sustain     :: timeout()
		rampdown    :: timeout()
		deact       :: timeout()
		wait        :: timeout()
		repeat      :: integer() -1 = pulse forever
		feedback    :: boolean()
		min_value   :: uint32()
		max_value   :: uint32()
        ramp_min    :: uint32() minimum time quant (hard is 20 ms)


    action() :: plugin_action() | [{pattern(),plugin_action()}].
	plugin_action() :: {App:app(),Flags::[plugin_output_flag()]}.
    plugin_output_flag() :: atom() | {atom(),term()}.


# Transmit processing

   transmit() ::
       {transmit,
	    Label::id(),
		{App::atom(), Flags::[{atom(),term()}]},
		Pattern::event_pattern()}.
		
#Common types

	id() :: atom() | integer().

	out_id() :: 1..254

	base_pattern() ::
        unsigned32() |
        digital | analog | encoder | rfid |
        variable() |
		{mask,Mask::unsigned32(),Match::unsigned32()} |
		{range,Low::integer(),High::integer()} |
		{'not',base_pattern()} |
		{'and',base_pattern(),base_pattern()} |
		{'or',base_pattern(),base_pattern()}.

    variable() :: atom().
		
    pattern() :: base_pattern() | [base_pattern()].

    event_pattern() ::
       { ID::pattern(), Chan::pattern(), Type::pattern(), Value::pattern() }


	can_id() :: uint11() | {cobid,uint11()} | {cobid,func(),ID::uint7()} |
		{xcobid,func(),ID::uint25()} | {xcobid,uint29()}.
	         
	func() :: nmt | sync | time_stamp |
    	      pdo1_tx | pdo1_rx |
    	      pdo2_tx | pdo2_rx |
    	      pdo3_tx | pdo3_rx |
    	      pdo4_tx | pdo4_rx |
    	      sdo_tx | sdo_rx |
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

	{input,in1,
		{{xcobid, pdo1_tx, 16#20001}, 20, digital, []},
		[springback],
		[1]}.

	{input,in2,
		{{xcobid, pdo1_tx, 16#20001}, 21, [], []},
		[],
		[2]}.

    {input,in3,
		{{xcobid, pdo1_tx, 16#20001}, 12, [], []},
		[],
		[3]}.

    {input,in4,
		{{xcobid, pdo1_tx, 16#20001},  4, [], []},
		[], [4]}.

    {input,in5,
		{{xcobid, pdo1_tx, 16#20001},  5, digital, 1, []},
		[2,3,4]}.

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

Distribution rules - this rule send all matching signals over can bus

    {transmit, 1,
        {hex_can, []};
        {[], [], [], []}}.
		
The account info for email is located in sys.config as
plugin app hex\_email data.

    {hex_email, foo, [
      {relay, "smtp.mail.com"},
      {username, "user"},
      {password, "password"},
      {port,2525}]}.
