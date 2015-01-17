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
       [input_flag()]
      }

    user_target() :: atom()
	builtin_target() :: value|inhibit|delay|rampup|sustain|rampdown|
                       deact|wait|repeat
	target() :: builtin_target() | user_target()

    input_flag() ::
		{springback,boolean()}   |
		{push_encoder,boolean()} |
	    {analog_to_digital,boolean()} |
	    {digital_to_analog,boolean()} |
		{invert,boolean()} |
	    {on_only,boolean()}  |
	    {off_only,boolean()} |
		{inc_encoder,boolean()} |
		{dec_encoder,boolean()} |
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
		{analog_offs, int32()} |
		{analog_scale, int32()}
		{target,[{channel,out_id()},{target,target()}]}

#Output processing

    output() ::
      {output,
       Label::out_id(),
       [output_flag()],
	   action()
      }

	output_flag() ::
		{nodeid, uint32()} |
		{chan, uint8()} |
        {ramp_min, uint32()} | %%  minimum time quant (hard is 20 ms)
		{min_value, uint32()} | %% = {target,[{name,value},{out_min,Value}]}
		{max_value, uint32()} | %% = {target,[{name,value},{out_max,Value}]}
		{value, uint32()} |
	    {inhbit, timeout()} |
		{delay, timeout()} |
	    {rampup, timeout()} |
		{sustain, timeout()} |
		{rampdown, timeout()} |
		{deact, timeout()} |
		{wait, timeout()} |
		{repeat, integer()} | %% -1 = pulse forever
		{feedback, boolean()} |
		{transmit, boolean()} |
		{digital, boolean()} | %% output digital signal otherwise analog
		{enable, bool_expr()} |   %% (default="value")
		{disable, bool_expr()} |  %% (default="not enable")
		{activate, bool_expr()} | %% (default="value")
		{deactivate, bool_expr()} | %% (default="not activate")
		{output, expr()} | %% output expression (default ="value")
        {target, target_spec()}

    target_spec() :: [target_flag()]

	target_flag() ::
		{name, atom()} |
		{type, clamp | wrap} |
		{in_min, uint32()} |
		{in_max, uint32()} |
		{out_min, uint32()} |
		{out_max, uint32()}.

    action() :: plugin_action() | [{cond(),plugin_action()}].
	plugin_action() :: {App:app(),Flags::[plugin_output_flag()]}.
    plugin_output_flag() :: atom() | {atom(),term()}.

	cond() :: [] | lexpr().  %% [] == true, default = "!value"

	active_expr() :: string() or expr().
	bool_expr() :: string() | lexpr().
	expr() :: lexpr().
	
	lexpr() ::
		aexpr() |
		rexpr() |
	    lexpr() and lexpr() |
	    lexpr() or lexpr() |
	    lexpr() xor lexpr() |
		not lexpr().
	rexpr() ::
		aexpr() < aexpr() |
		aexpr() =< aexpr() |
		aexpr() > aexpr() |
		aexpr() >= aexpr() |
		aexpr() == aexpr() |
		aexpr() /= aexpr().
    aexpr() ::
	    atom() | %% target variable
		
		integer() |
		true | %% = 1
		false |  %% = 0
	    aexpr() + aexpr() |
	    aexpr() - aexpr() |
	    aexpr() * aexpr() |
	    aexpr() div aexpr() |
	    aexpr() rem aexpr() |
		- aexpr().

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
        uint32() |
        digital | analog | encoder | rfid |
        variable() |
		{mask,Mask::uint32(),Match::uint32()} |
		{range,Low::integer(),High::integer()} |
		{'not',base_pattern()} |
		{'and',base_pattern(),base_pattern()} |
		{'or',base_pattern(),base_pattern()}.

    variable() :: atom().
		
    pattern() :: base_pattern() | [base_pattern()].

    event_pattern() ::
       { ID::pattern(), Chan::pattern(), Type::pattern(), Value::pattern() }


	can_id() ::
       uint11() |
       {cobid,uint11()} |
       {cobid,self} |
       {cobid,func(),ID::uint7()} |
       {cobid,func(),self} |
       {xcobid,func(),ID::uint25()} |
       {xcobid,func(),self} |
       {xcobid,uint29()} |
       {xcobid,self}
	         
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

	{output, 1, [],
		{hex_gpio, [{board, cpu}, {pin_reg,0}, {pin, 17}]} }.

	{output, 2, [],
		{hex_gpio, [{board, piface}, {pin_reg,0}, {pin, 1}]} }.

    {output, 3, [{inhibit,10000}],
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

	{output, 4, [{analog_min,0},{analog_max,10},{style, smooth}],
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
