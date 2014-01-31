%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Input match processing
%%% @end
%%% Created : 22 Jan 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_input).

-include("../include/hex.hrl").

run(Signal, Rules) when is_record(Signal, hex_signal) ->
    run_(Signal, Rules).

run_(Signal, [Rule|Rules]) ->
    case match_rule(Signal, Rule) of
	{true,Value} ->
	    case Signal#hex_signal.type of
		?HEX_DIGITAL ->
		    send_output(Input#input_rule.output, {digital,Value});
		?HEX_ANALOG -> 
		    send_output(Input#input_rule.output, {analog,Value});
		?HEX_ENCODER -> 
		    send_output(Input#input_rule.output, {encoder,Value});
		Type ->
		    send_output(Input#input_rule.output, {Type,Value});
	    end,
	    run_(Frame, Rules);
	false ->
	    run_(Frame, Rules)
    end;
run_(_Frame, []) ->
    ok.

send_output([I|Is], Value) ->
    index_to_pid(I) ! Value,
    send_output(Is);
send_output([], _) ->
    ok.

index_to_pid(I) ->
    get({output,I}).

match_rule(Signal, Rule) ->
    case match_(Signal#hex_signal.id, Rule#hex_rule.id) andalso 
	match_(Signal#hex_signal.chan, Rule#hex_rule.chan) andalso 
	match_(Signal#hex_signal.type, Rule#hex_rule.type) of
	true ->
	    case match_(Signal#hex_signal.value Rule#hex_rule.value) of
		true -> {true, Signal#hex_signal.value};
		false -> false
	    end;
	false ->
	    false
    end.

match_(A, A) -> true;
match_({mask,Mask,Match}, A) -> A band Mask =:= Match;
match_({not_mask,Mask,Match}, A) -> not (A band Mask =:= Match);
match_({range,Low,High}, A) -> (A >= Low) andalso (A =< High);
match_([Cond|Cs], A) -> match_(Cond,A) andalso match_(Cs, A);
match_([], _A) -> true.

