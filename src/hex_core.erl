%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    hex reactive core
%%% @end
%%% Created : 19 Jun 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_core).

-export([scan/1, parse/1, parse_tokens/1]).

-export([new/0]).
-export([add_expr/2, add_variable/2, add_variables/2]).
-export([eval/1]).
-export([set_value/3, set_values/2]).
-export([value/2, tick/2]).
-export([dump/1]).

-define(dbg(F,A), io:format((F),(A))).
%%-define(dbg(F,A), ok).

-type variable() :: integer() | atom().

%%
-record(core,
	{
	  next_var  = 0,
	  tick = 0,
	  %% expr: sub expression store Var->Ci|{Op,Ai}->Ci|{Op,Ai,Bi}->Ci
	  expr = dict:new() :: dict:dict(),
	  %% rules: rules Ci -> {Op,Ai,Bi},  Ci -> {Op, Ai}
	  rules = array:new() :: array:array(),
	  %% refs: Xi -> [C1,..Cn]  rules Cj referring to Xi
	  refs = dict:new() :: dict:dict(),
	  %% value: Xi -> Value
	  values = dict:new() ::  dict:dict(),
	  %% prev: Xi -> Value (values in previous tick)
	  prev = dict:new() ::  dict:dict(),
	  %% tick:  Xi -> Tick
	  ticks = dict:new() :: dict:dict(),
	  %% queue set of variables already in queue
	  queue_set = sets:new() :: sets:set(),
	  %% queue of Ci rules to be executed
	  queue = queue:new() :: queue:queue()
	}).

%%
%% Create a new core
%%
new() ->
    #core{}.

%%
%% Simple parse using erlang expressions
%%
parse(String) when is_list(String) ->
    Ts = scan(String),
    parse_tokens(Ts).

parse_tokens(Ts) ->
    {ok,[Expr]} = erl_parse:parse_exprs(Ts),
    rewrite_expr(Expr).

%%
%% Simple token scanner using erlang scanner
%%
scan(String) ->
    {ok,Ts0,_} = erl_scan:string(String),
    Ts = rewrite_tokens(Ts0, []),
    Ts ++ [{dot,1}].

%%
%% token translations:
%%    '=>'       -> '>='
%%    '<='       -> '=<'
%%    '?' x      -> updated ( x )
%%    '?' '?' x  -> changed ( x )
%%    '!'        -> 'not'
%%    '!' '='    -> '/='
%%    '&' '&'    ->  'and'
%%    '||'       ->  'or'
%%    '@' x      -> prev ( x )
%%
rewrite_tokens([T|Ts],Acc) ->
    case T of
	{'=>',Ln} -> rewrite_tokens(Ts, [{'>=',Ln}|Acc]);
	{'<=',Ln} -> rewrite_tokens(Ts, [{'=<',Ln}|Acc]);
	{'!',Ln} ->
	    case Ts of
		[{'=',Ln}|Ts1] ->
		    rewrite_tokens(Ts1,  [{'/=',Ln}|Acc]);
		_ ->
		    rewrite_tokens(Ts,  [{'not',Ln}|Acc])
	    end;
	{'||',Ln} -> rewrite_tokens(Ts, [{'or',Ln}|Acc]);
	{'&',Ln} ->
	    case Ts of 
		[{'&',Ln}|Ts1] -> rewrite_tokens(Ts1, [{'and',Ln}|Acc]);
		_ -> rewrite_tokens(Ts, [{'&',Ln}|Acc])
	    end;
	{'?',Ln} ->
	    case Ts of
		[{atom,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{atom,Ln,updated} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);
		[{var,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{var,Ln,updated} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);

		[{'?',_Ln},{atom,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{atom,Ln,changed} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);
		[{'?',_Ln},{var,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{var,Ln,changed} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);
		_ ->
		    rewrite_tokens(Ts, [{'?',Ln}|Acc])
	    end;
	{'@',Ln} ->
	    case Ts of
		[{atom,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{atom,Ln,prev} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);
		[{var,Ln,X}|Ts1] ->
		    Acc1 = [{')',Ln},{atom,Ln,X},{'(',Ln},{var,Ln,prev} |
			    Acc],
		    rewrite_tokens(Ts1, Acc1);
		_ ->
		    rewrite_tokens(Ts, [{'@',Ln}|Acc])
	    end;
	_ ->
	    rewrite_tokens(Ts, [T|Acc])
    end;
rewrite_tokens([], Acc) ->
    lists:reverse(Acc).
    
%%
%% Simplify expression
%%
rewrite_expr({atom,_,true})  -> {const,1};
rewrite_expr({atom,_,false}) -> {const,0};
rewrite_expr({atom,_,Target}) when is_atom(Target) -> Target;
rewrite_expr({var,_,Target}) when is_atom(Target) -> Target;
rewrite_expr({integer,_,Value}) -> {const,Value};

rewrite_expr({op,_,'and',A,B}) -> {'and',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'or',A,B}) ->  {'or',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'xor',A,B}) ->  {'xor',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'not',A}) ->  {'not',rewrite_expr(A)};

%% boolean expression updated(x) and changed(x)  (?x and ??x)
rewrite_expr({call,_,{atom,_,updated},[A]}) -> {updated, rewrite_expr(A)};
rewrite_expr({call,_,{atom,_,changed},[A]}) -> {changed, rewrite_expr(A)};

%% arithmetical
rewrite_expr({op,_,'+',A,B}) -> {'+',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'-',A,B}) ->  {'-',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'*',A,B}) ->  {'*',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'div',A,B}) -> {'div',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'rem',A,B}) -> {'rem',rewrite_expr(A),rewrite_expr(B)};

rewrite_expr({call,_,{atom,_,max},[A,B]}) ->
    {'max',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({call,_,{atom,_,min},[A,B]}) ->
    {'min',rewrite_expr(A),rewrite_expr(B)};

rewrite_expr({op,_,'-',A}) -> {'-',rewrite_expr(A)};

%% previous value
rewrite_expr({call,_,{atom,_,prev},[{atom,_,X}]}) -> {'prev',X};
rewrite_expr({call,_,{atom,_,prev},[{var,_,X}]}) -> {'prev',X};
%% tick
rewrite_expr({call,_,{atom,_,tick},[]}) -> {'tick'};
rewrite_expr({call,_,{atom,_,tick},[{atom,_,X}]}) -> {'tick',X};
rewrite_expr({call,_,{atom,_,tick},[{var,_,X}]}) -> {'tick',X};

%% match - assign variables
rewrite_expr({match,_,{atom,_,A},B}) -> {'=',A,rewrite_expr(B)};
rewrite_expr({match,_,{var,_,A},B}) -> {'=',A,rewrite_expr(B)};
rewrite_expr({match,_,A,B}) -> {'==',rewrite_expr(A),rewrite_expr(B)};

%% comparison
rewrite_expr({op,_,'==',A,B}) -> {'==',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'/=',A,B}) -> {'/=',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'=<',A,B}) -> {'=<',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'<',A,B}) -> {'<',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'>=',A,B}) -> {'>=',rewrite_expr(A),rewrite_expr(B)};
rewrite_expr({op,_,'>',A,B}) -> {'>',rewrite_expr(A),rewrite_expr(B)}.

%% install variable into value set
-spec add_variable(Var::atom(), R::#core{}) -> #core{}.

add_variable(Var, Core) when is_atom(Var), is_record(Core, core) ->
    case dict:find(Var, Core#core.expr) of
	error ->
	    {_Vi,Core1} = new_var_(Var, 0, Core),
	    Core1;
	{ok,_Vi} ->
	    Core
    end.

-spec add_variables([Var::atom()], Core::#core{}) -> #core{}.

add_variables([Var|Vs], Core) ->
    Core1 = add_variable(Var, Core),
    add_variables(Vs, Core1);
add_variables([], Core) ->
    Core.

%% install expression into rule set
-spec add_expr(Expr::term(), R::#core{}) -> {Var::variable(), #core{}}.

add_expr({updated,X},Core) ->
    add_expr({'==',{tick,X},{tick}}, Core);
add_expr({changed,X},Core) ->
    add_expr({'/=',{prev,X},X}, Core);
add_expr(Expr, Core) ->
    case Expr of
	{Op,A,B} ->
	    {Ai,CoreA} = add_expr(A,Core),
	    {Bi,CoreB} = add_expr(B,CoreA),
	    add_expr_({Op,Ai,Bi},CoreB);
	C={const,_} -> 
	    {C,Core};
	{Op,A} ->
	    {Ai,CoreA} = add_expr(A,Core),
	    add_expr_({Op,Ai},CoreA);
	{Op} ->
	    add_expr_({Op},Core);
	Var when is_atom(Var) ->
	    case dict:find(Var, Core#core.expr) of
		error ->
		    new_var_(Var, 0, Core);
		{ok,Vi} ->
		    {Vi,Core}
	    end
    end.

add_expr_(Node,R) ->
    case dict:find(Node, R#core.expr) of
	error ->
	    Ci = R#core.next_var,
	    ?dbg("add_expr_: ~w = ~w\n", [Ci, Node]),
	    Rules = array:set(Ci, Node, R#core.rules),
	    Expr  = dict:store(Node, Ci, R#core.expr),
	    Refs  = add_refs(Ci, Node, R#core.refs),
	    R1 = R#core { rules=Rules, expr=Expr, refs=Refs, next_var=Ci+1 },
	    R2 = enqueue_([Ci],R1#core.queue,R1#core.queue_set,R1),
	    {Ci, R2};
	{ok,Ci} ->
	    {Ci, R}
    end.

%% build cross reference
add_refs(Ci, {_Op,Ai,Bi}, Refs) ->
    Refs1=dict:append(Ai, Ci, Refs),
    dict:append(Bi, Ci, Refs1);
add_refs(Ci, {prev,Ai}, Refs) ->
    Refs1=dict:append(Ai, Ci, Refs),
    dict:append('_prev', Ci, Refs1);
add_refs(Ci, {tick,Ai}, Refs) ->
    Refs1=dict:append(Ai, Ci, Refs),
    dict:append('_tick', Ci, Refs1);
add_refs(Ci, {_Op,Ai}, Refs) ->
    dict:append(Ai, Ci, Refs);
add_refs(Ci, {tick}, Refs) ->
    dict:append('_tick', Ci, Refs);
add_refs(_Ci, _Expr, Refs) ->
    Refs.

%% get current value
-spec value(Var::variable(), Core::#core{}) -> integer().

value(Var, Core) when is_atom(Var), is_record(Core, core) ->
    case dict:find(Var, Core#core.expr) of
	error -> 0;
	{ok,Vi} -> dict:fetch(Vi, Core#core.values)
    end;
value(Vi, Core) when is_integer(Vi), is_record(Core, core) ->
    dict:fetch(Vi, Core#core.values).

%% get current tick
-spec tick(Var::variable(), Core::#core{}) -> integer().

tick(Var, Core) when is_atom(Var), is_record(Core, core) ->
    dict:fetch(Var, Core#core.ticks).


%% set value and enqueue dependecies
-spec set_value(Var::variable(), Value::integer(), Core::#core{}) -> #core{}.
set_value(Var, Value, Core) when is_atom(Var), is_record(Core, core) ->
    set_value_(Var, Value, Core).

set_value_(Var, Value, Core) ->
    case dict:find(Var, Core#core.expr) of
	error ->
	    {_Vi,Core1} = new_var_(Var, Value, Core),
	    Core1;
	{ok,Vi} ->
	    Values = store_value_(Vi, Value, Core#core.values),
	    Ticks  = store_tick_(Vi, Core#core.tick, Core#core.ticks),
	    enqueue_(Vi, Core#core { values=Values, ticks=Ticks })
    end.

-spec set_values([{Var::variable(),Value::integer()}],Core::#core{}) -> #core{}.

set_values(List, Core) when is_list(List), is_record(Core, core) ->
    set_values_(List, Core).

set_values_([{Var,Value}|List], Core) when is_atom(Var), is_integer(Value) ->
    Core1 = set_value_(Var,Value,Core),
    set_values_(List, Core1);
set_values_([], Core) ->
    Core.
    
%% enqueue all variables that reference Var
enqueue_(Var, Core) ->
    enqueue_(refs(Var, Core#core.refs),
	     Core#core.queue,Core#core.queue_set, Core).

enqueue_([Yi|Ys], Queue, QSet, Core) ->
    case sets:is_element(Yi, QSet) of
	true -> 
	    enqueue_(Ys, Queue, QSet, Core);
	false ->
	    ?dbg("enqueue: ~w\n", [Yi]),
	    enqueue_(Ys, queue:in(Yi,Queue), sets:add_element(Yi,QSet), Core)
    end;
enqueue_([], Queue, QSet, Core) ->
    Core#core { queue = Queue, queue_set = QSet }.

    
%% Run eval.
-spec eval(Core::#core{}) -> #core{}.

eval(Core) ->
    RefList = refs('_prev',Core#core.refs) ++ refs('_tick',Core#core.refs),
    %% RefList = [],
    %% always push _prev and _tick expression if available since they
    %% must be re-evaluated each eval/tick.
    eval_enqueue_(RefList,
		  Core#core.queue, Core#core.queue_set,
		  Core#core.values, Core#core.ticks,
		  Core#core.tick, Core).

eval_queue(Queue, QSet, Values, Ticks, CurrentTick, Core) ->
    case queue:out(Queue) of
	{{value,Xi}, Queue1} ->
	    ?dbg("execute rule: ~w\n", [Xi]),
	    case array:get(Xi, Core#core.rules) of
		{'=',Ai,Bi} ->
		    Value = value_(Bi,Values),
		    Values1 = store_value_(Ai, Value, Values),
		    Ticks1 = store_tick_(Ai, CurrentTick, Ticks),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, Value, Values1),
				 store_tick_(Xi, CurrentTick, Ticks1),
				 CurrentTick, Core);
		{Op,Ai,Bi} ->
		    A = value_(Ai,Values),
		    B = value_(Bi,Values),
		    Value = ev_expr(Op,A,B),
		    ?dbg("{~w,~w/~w,~w/~w} = ~w\n", [Op,Ai,A,Bi,B,Value]),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, Value, Values),
				 store_tick_(Xi, CurrentTick, Ticks),
				 CurrentTick, Core);
		{prev,Ai} ->
		    Value = prev_(Ai,Core#core.prev),
		    ?dbg("{prev,~w} = ~w\n", [Ai,Value]),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, Value, Values),
				 store_tick_(Xi, CurrentTick, Ticks),
				 CurrentTick, Core);
		{tick} ->
		    ?dbg("tick() = ~w\n", [CurrentTick]),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, CurrentTick, Values),
				 store_tick_(Xi, CurrentTick, Ticks),
				 CurrentTick, Core);
		{tick,Ai} ->
		    Value = tick_(Ai,Ticks),
		    ?dbg("tick(~w) = ~w\n", [Ai,Value]),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, Value, Values),
				 store_tick_(Xi, CurrentTick, Ticks),
				 CurrentTick, Core);
		{Op,Ai} ->
		    A = value_(Ai,Values),
		    Value = ev_expr(Op,A),
		    ?dbg("{~w,~w/~w} = ~w\n", [Op,Ai,A,Value]),
		    eval_enqueue(Xi,Queue1,QSet,
				 store_value_(Xi, Value, Values),
				 store_tick_(Xi, CurrentTick, Ticks),
				 CurrentTick, Core)
	    end;
	{empty, Queue1} ->
	    Core#core { queue = Queue1, queue_set = sets:new(),
			tick = CurrentTick+1,
			prev = Values, %% previous values in next round
			ticks = Ticks, values = Values }
    end.

eval_enqueue(Xi,Queue,QSet,Values,Ticks,CurrentTick,Core) ->
    RefList = refs(Xi,Core#core.refs),
    eval_enqueue_(RefList,Queue,QSet,Values,Ticks,CurrentTick,Core).

%% enqueue all references not already in queue or already processed
eval_enqueue_([Yi|Ys],Queue,QSet,Values,Ticks,CurrentTick,Core) ->
    case sets:is_element(Yi, QSet) of
	true -> 
	    eval_enqueue_(Ys,Queue,QSet,Values,Ticks,CurrentTick,Core);
	false ->
	    ?dbg("eval_enqueue: ~w\n", [Yi]),
	    eval_enqueue_(Ys,queue:in(Yi,Queue),sets:add_element(Yi,QSet),
			  Values,Ticks,CurrentTick,Core)
    end;
eval_enqueue_([],Queue,QSet,Values,Ticks,CurrentTick,Core) ->
    eval_queue(Queue,QSet,Values,Ticks,CurrentTick,Core).

%% push all references not already in queue or already processed
eval_enqueue_r_([Yi|Ys],Queue,QSet,Values,Ticks,CurrentTick,Core) ->
    case sets:is_element(Yi, QSet) of
	true -> 
	    eval_enqueue_r_(Ys,Queue,QSet,Values,Ticks,CurrentTick,Core);
	false ->
	    ?dbg("eval_enqueue_r: ~w\n", [Yi]),
	    eval_enqueue_r_(Ys,queue:in_r(Yi,Queue),sets:add_element(Yi,QSet),
			    Values,Ticks,CurrentTick,Core)
    end;
eval_enqueue_r_([],Queue,QSet,Values,Ticks,CurrentTick,Core) ->
    eval_queue(Queue,QSet,Values,Ticks,CurrentTick,Core).

refs(Var, Dict) ->    
    case dict:find(Var, Dict) of
	error -> [];
	{ok,Refs} -> Refs
    end.
    
%% fetch a tick (last updated tick) or return -1 if not found
tick_(Var, Dict) ->
    case dict:find(Var, Dict) of
	error ->
	    -1;
	{ok,LastTick} ->
	    LastTick
    end.

%% store a value
store_value_(Var, Value, Dict) ->
    ?dbg("store_value_: var(~w)=~w\n", [Var, Value]),
    dict:store(Var, Value, Dict).

%% store a tick
store_tick_(Var, Tick, Dict) ->
    ?dbg("store_tick_: tick(~w)=~w\n", [Var, Tick]),
    dict:store(Var, Tick, Dict).

%% fetch a value or report error and return 0 if not found
value_({const,Value}, _Dict) -> Value;
value_(Var, Dict) -> dict:fetch(Var, Dict).

%% special since previous of 
prev_(Var, Dict) ->
    case dict:find(Var,Dict) of
	error -> 
	    io:format("prev_: prev(~w)=undefined\n", [Var]),
	    0;
	{ok,Val} -> Val
    end.

%% add var when it is known not to exist in variables dict
new_var_(Var, Init, Core) ->
    Vi = Core#core.next_var,
    ?dbg("new_var_: ~w/~w = ~w\n", [Var, Vi, Init]),
    Vars = dict:store(Var, Vi, Core#core.expr),
    Rules = array:set(Vi, Var, Core#core.rules),
    Vals = store_value_(Vi, Init, Core#core.values),
    Ticks  = store_tick_(Vi, Core#core.tick, Core#core.ticks),
    {Vi,Core#core { expr=Vars, rules=Rules, values=Vals, 
		    ticks=Ticks, next_var=Vi+1 }}.

%% dump core structure for debugging
dump(Core) when is_record(Core, core) ->
    io:format("BEGIN CORE\n", []),
    io:format("  tick: ~w\n", [Core#core.tick]),
    io:format("  next_var: ~w\n", [Core#core.next_var]),
    io:format("     prevs: ~w\n", [refs('_prev',Core#core.refs)]),
    io:format("     ticks: ~w\n", [refs('_tick',Core#core.refs)]),
    io:format("  RULES\n", []),
    array:foldl(
      fun(I,Expr,_Acc) ->
	      Refs = refs(I, Core#core.refs),
	      io:format("    ~w = ~w => ~w\n", [I,Expr, Refs])
      end, ok, Core#core.rules),
    io:format("  VALUES\n", []),
    lists:foreach(
      fun(Vi) ->
	      Val  = dict:fetch(Vi, Core#core.values),
	      Tick = dict:fetch(Vi, Core#core.ticks),
	      Refs = refs(Vi, Core#core.refs),
	      io:format("    ~w = ~w [t=~w] => ~w\n", [Vi, Val, Tick, Refs])
      end, dict:fetch_keys(Core#core.values)),
    io:format("  EXPRS\n", []),
    dict:fold(fun(Expr,I,_) ->
		      io:format("    ~w -> ~w\n", [Expr,I])
	       end, ok, Core#core.expr),
    io:format("END\n").

    
ev_expr('+',A,B) -> A+B;
ev_expr('-',A,B) -> A-B;
ev_expr('*',A,B) -> A*B;
ev_expr('div',A,B) -> A div B;
ev_expr('rem',A,B) -> A rem B;
ev_expr('min',A,B) -> min(A,B);
ev_expr('max',A,B) -> max(A,B);
ev_expr('and',A,B) -> min(A,B);
ev_expr('or',A,B)  -> max(A,B);
ev_expr('xor',A,B) -> 1-ev_bool(A =:= B);
ev_expr('==',A,B) -> ev_bool(A =:= B);
ev_expr('/=',A,B) -> ev_bool(A =/= B);
ev_expr('=<',A,B) -> ev_bool(A =< B);
ev_expr('<',A,B) -> ev_bool(A < B);
ev_expr('>',A,B) -> ev_bool(A > B);
ev_expr('>=',A,B) -> ev_bool(A >= B).
    
ev_expr('not',A) -> ev_bool(A =:= 0);
ev_expr('-',A) -> -A.

ev_bool(true) -> 1;
ev_bool(false) -> 0.
