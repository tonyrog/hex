Description of input and output logic
=====================================

Output logic
============

While output is enabled the signals may generate output that
is sent as output, generate feedback, run actions or all of them.

The logic for signals to be passed to actions or output is process
as follows:

First activation of outputs

    if (!active) {
      if (eval(activate_expr)) {
      	 active = 1;
      	 transmit_active(true);
    }
    else {
      if (eval(deactivate_expr))
      	 transmit_active(false);
         active = 0;
      }
    }

Then check if output is enabled or disabled

    if (!enabled) {
      enabled = eval(enable_expr);
      if (!enabled)
      	 return 0;
    }
    else {
      if (eval(disabled_expr)) {
         enabled = 0;
	 return 0;
      }
    }
    return 1;

Produce output and actions

    if (enabled || prev_enable) {
       output = eval(output_expr);
       for (a in actions) {
       	   if (eval(a.cond))
	      execute(a.action);
      }
      transmit_value(output);
    }

Defaults

    enable_expr   = "in"
    disabled_expr = "not in"
    active_expr   = "in"
    deactive_expr = "not in"
    output_expr   = "out"

The 'in' is the default digital signal value. The default
analog value as the name 'an'. Other signals must have
the target name attached to it.
'out' variable is the value produce by the output function.


Other signals are declared as targets in the output section
like:

	[{target,[{name,x},{value,"clamp(x,1,10)"}]},
	 {target,[{name,y},{value,"map(y,0,255,1,10)"}]},
	 {target,[{name,z},{value,"z+x+@y"}]}
	]

Output standard targets:
       name		default
       in		"0"		default digital input
       an		"0"		default analog input
       low		"0"		low output value
       high		"1023"		hight output value
       inhibit		"0"		inhibit time
       delay    	"0"		activation delay
       rampup   	"0"		rampup time
       sustain  	"0"		sustain time
       rampdown 	"0"		rampdown time
       deact    	"0"		deactivation delay
       wait     	"0"		wait before repeat
       repeat   	"0"		number of repeats
       feedback 	"0"		feedback flag
       transmit 	"0"		transmit flag


Conditional expression

To aid conditional expression and trigger correct behavior
the following operators are provided

    ?x             x was written to, maybe same value, since last eval
    ??x            x has changed value since last evaluation
    @x             value of x in previous eval
    (x != @x)      same as ??x
    x - @x	   value delta change = (x - prev(x))
