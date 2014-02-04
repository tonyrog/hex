%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Input match processing
%%% @end
%%% Created : 22 Jan 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_input).

-include("../include/hex.hrl").

-export([run/3]).

run(Signal, Rules, OutFun) when is_record(Signal, hex_signal) ->
    run_(Signal, Rules, OutFun).

run_(Signal, [Rule|Rules], OutFun) ->
    case match_rule(Signal, Rule) of
	{true,Value} ->
	    Src = Signal#hex_signal.source,
	    V = case Signal#hex_signal.type of
		    ?HEX_DIGITAL -> {digital,Value,Src};
		    ?HEX_ANALOG ->  {analog,Value,Src};
		    ?HEX_ENCODER -> {encoder,Value,Src};
		    Type -> {Type,Value,Src}
		end,
	    [OutFun(Out,V) || Out <- Rule#hex_rule.output],
	    run_(Signal, Rules, OutFun);
	false ->
	    run_(Signal, Rules, OutFun)
    end;
run_(_Signal, [], _OutFun) ->
    ok.

match_rule(Signal, Rule) ->
    case match_(Signal#hex_signal.id, Rule#hex_rule.id) andalso 
	match_(Signal#hex_signal.chan, Rule#hex_rule.chan) andalso 
	match_(Signal#hex_signal.type, Rule#hex_rule.type) of
	true ->
	    case match_(Signal#hex_signal.value, Rule#hex_rule.value) of
		true -> {true, Signal#hex_signal.value};
		false -> false
	    end;
	false ->
	    false
    end.

match_(A, A) -> true;
match_({mask,Mask,Match}, A) -> A band Mask =:= Match;
match_({range,Low,High}, A) -> (A >= Low) andalso (A =< High);
match_({'not',Cond}, A) -> not match_(Cond,A);
match_({'and',C1,C2}, A) -> match_(C1,A) andalso match_(C2,A);
match_({'or',C1,C2}, A) -> match_(C1,A) orelse match_(C2,A);
match_([Cond|Cs], A) -> match_(Cond,A) andalso match_(Cs, A);
match_([], _A) -> true.
