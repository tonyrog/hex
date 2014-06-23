-module(hex_core_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

simple_expr1_test() -> 8 = simple_eval("a+b", [{a,5},{b,3}]).
simple_expr2_test() -> 15 = simple_eval("a*b", [{a,5},{b,3}]).
simple_expr3_test() -> 2 = simple_eval("a-b", [{a,5},{b,3}]).
simple_expr4_test() -> 1 = simple_eval("a div b", [{a,5},{b,3}]).
simple_expr5_test() -> 2 = simple_eval("a rem b", [{a,5},{b,3}]).
simple_expr6_test() -> 3 = simple_eval("min(a,b)", [{a,5},{b,3}]).
simple_expr7_test() -> 5 = simple_eval("max(a,b)", [{a,5},{b,3}]).
simple_expr8_test() -> -5 = simple_eval("-a", [{a,5},{b,3}]).
simple_expr9_test() -> 0 = simple_eval("a<b", [{a,5},{b,3}]).
simple_expr10_test() -> 0 = simple_eval("a<=b", [{a,5},{b,3}]).
simple_expr11_test() -> 1 = simple_eval("a>b", [{a,5},{b,3}]).
simple_expr12_test() -> 1 = simple_eval("a>=b", [{a,5},{b,3}]).
simple_expr13_test() -> 0 = simple_eval("a==b", [{a,5},{b,3}]).
simple_expr14_test() -> 1 = simple_eval("a!=b", [{a,5},{b,3}]).

simple_expr15_test() -> 1 = simple_eval("(a<b) || (b>2)", [{a,5},{b,3}]).
simple_expr16_test() -> 1 = simple_eval("(a<b) or (b>2)", [{a,5},{b,3}]).
simple_expr17_test() -> 0 = simple_eval("(a<b) && (b>2)", [{a,5},{b,3}]).
simple_expr18_test() -> 0 = simple_eval("(a<b) and (b>2)", [{a,5},{b,3}]).
simple_expr19_test() -> 1 = simple_eval("(a<b) xor (b>2)", [{a,5},{b,3}]).
simple_expr20_test() -> 1 = simple_eval("!(a<b) xor !(b>2)", [{a,5},{b,3}]).
simple_expr21_test() -> 1 = simple_eval("not (a<b) xor not (b>2)",[{a,5},{b,3}]).

%% test ? update expression 
multi_expr1_test() ->
    [1,0,1,0] = multi_eval("?x", [ [{x,0}], [{y,2}], [{x,1}], [{y,0}]]).

%% test ?? value changed expression 
multi_expr2_test() ->
    [0,1,0,1] = multi_eval("??x", [ [{x,0}], [{x,1}], [{x,1}], [{x,2}]]).

multi_expr3_test() ->
    [1,0,1,0] = multi_eval("not ??x", [ [{y,0}], [{x,1}], [{y,1}], [{x,2}]]).

multi_expr4_test() ->
    [1,2,3,4] = multi_eval("a", [ [{a,1}],[{a,2}],[{a,3}],[{a,4}]]).

multi_expr5_test() ->
    [0,1,2,3] = multi_eval("prev(a)", [ [{a,1}],[{a,2}],[{a,3}],[{a,4}]]).

multi_expr6_test() ->
    [0,4,7] = multi_eval("prev(x)", [ [{x,4}],[{x,7}],[{x,7}]]).

multi_expr7_test() ->
    [4,7,7] = multi_eval("x", [ [{x,4}],[{x,7}],[{x,7}]]).

multi_expr8_test() ->
    %% must evaluate prev(x) before (x != prev(x)) ???
    [1,1,0] = multi_eval("x != prev(x)",[[{x,4}],[{x,7}],[{x,7}]]).

multi_expr10_a_test() ->
    [1,0,1] = multi_eval("(t=a*b) != prev(t)", 
			 [ [{a,3},{b,5}],
			   [{c,7}],
			   [{a,2}]
			 ]).

multi_expr10_test() ->
    [1,0] = multi_eval("changed(a*b)", 
		       [ [{a,3},{b,5}],
			 [{c,7}] 
		       ]).

multi_expr11_test() ->
    [1,0,1] = multi_eval("updated(a*b)", [ [{a,3},{b,5}], [{c,7}], [{a,2}] ]).

multi_expr12_test() ->
    [1,3,5,7] = multi_eval("a+prev(a)", [ [{a,1}],[{a,2}],[{a,3}],[{a,4}]]).

multi_expr13_test() ->
    [0,2,6,12] = multi_eval("a*prev(a)", [ [{a,1}],[{a,2}],[{a,3}],[{a,4}]]).

multi_expr14_test() ->
    [1,2,4,8,17,38,92,236] = multi_eval("x=a+prev(a)*prev(x)", 
					[ [{a,1}],[{a,2}],
					  [{a,3}],[{a,4}],
					  [{a,5}],[{a,6}],
					  [{a,7}],[{a,8}]
					]).

multi_expr15_test() ->
    [0,1,2,3] = multi_eval("tick()", [ [{a,7}],[{a,1}],[{a,3}],[{a,11}] ]).

multi_expr16_test() ->
    [1,1,1,1] = multi_eval("tick() == tick(a)", 
			   [ [{a,7}],[{a,1}],[{a,3}],[{a,11}] ]).


simple_eval(StrExpr, Values) ->
    R0 = hex_core:new(),
    {X,R1} = hex_core:add_expr(hex_core:parse(StrExpr), R0),
    R2 = hex_core:set_values(Values, R1),
    R3 = hex_core:eval(R2),
    hex_core:value(X, R3).


multi_eval(StrExpr, ValuesList) ->
    R0 = hex_core:new(),
    {X,R1} = hex_core:add_expr(hex_core:parse(StrExpr), R0),
    multi_eval_expr(X, R1, ValuesList, []).

multi_eval_expr(X, R, [Values|ValuesList], Acc) ->
    R1 = hex_core:set_values(Values, R),
    R2 = hex_core:eval(R1),
    V = hex_core:value(X, R2),
    multi_eval_expr(X, R2, ValuesList, [V|Acc]);
multi_eval_expr(_X, _R, [], Acc) ->
    lists:reverse(Acc).

%% interpret a list of actions
interp(Actions) ->
    interp(Actions, hex_core:new(), []).
    
interp([Expr | Es], R, Acc) ->
    case Expr of 
	{add,Expr} when is_list(Expr) ->
	    R1 = hex_core:add_expr(hex_core:parse(Expr), R),
	    interp(Es, R1, Acc);
	{add,Expr} when is_tuple(Expr) ->
	    R1 = hex_core:add_expr(Expr, R),
	    interp(Es, R1, Acc);
	{set, Var, Value} ->
	    R1 = hex_core:set_value(Var, Value, R),
	    interp(Es, R1, Acc);
	{set, VarValues} ->
	    R1 = hex_core:set_values(VarValues, R),
	    interp(Es, R1, Acc);
	{value,Var} ->
	    Value = hex_core:value(Var, R),
	    interp(Es, R, [Value|Acc]);
	{tick,Var} ->
	    Tick = hex_core:tick(Var, R),
	    interp(Es, R, [Tick|Acc]);
	eval ->
	    R1 = hex_core:eval(R),
	    interp(Es, R1, Acc)
    end.

	    
	    

    
