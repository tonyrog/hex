%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Ets tree 
%%% @end
%%% Created : 30 Mar 2015 by Tony Rogvall <tony@rogvall.se>

-module(hex_tree_db).

-export([new/2, lookup/2, insert/2, delete/1, delete/2,
	 first/1, last/1, next/2, prev/2,
	 first_child/2, last_child/2, next_sibling/2, prev_sibling/2,
	 foldl/3, foldr/3, foldbl/3, foldbr/3,
	 depth_first/3, breadth_first/3]).
-export([enql/3, enqr/3]).
-export([dump/1]).

-define(QUEUE_T(), term()).  %% R15 !
%% -define(QUEUE_T, quque:queue()).

-define(eot, '$end_of_table').
-define(is_external_key(Key), is_binary((Key))).
-define(is_internal_key(Key), (((Key) =:= []) 
			       orelse 
				 (is_list((Key)) andalso 
				  is_binary(hd((Key)))))).

-type eot() :: '$end_of_table'.
-type internal_key() :: [binary()].
-type external_key() :: binary() | string().
-type key() :: internal_key() | external_key().
-type table() :: term().
-type option() :: {atom(),term()}.


-spec new(Name::atom(), Options::[option()]) -> table().
new(Name, Options) ->
    ets:new(Name, Options++[ordered_set]).

-spec delete(Table::table()) -> true.
delete(Table) -> ets:delete(Table).

-spec insert(Table::table(), Elem::tuple()) -> true.
insert(T, Elem) ->
    Pos = ets:info(T, keypos),
    ets:insert(T, to_internal(Pos, Elem)).

-spec lookup(Table::table(), Key::key()) -> [term()].
lookup(Table, Key) ->
    case ets:lookup(Table,internal_key(Key)) of
	[Elem] ->
	    Pos = ets:info(Table, keypos),
	    [to_external(Pos,Elem)];
	[] ->
	    []
    end.

-spec delete(Table::table(), Key::key()) -> true.
delete(T, K) -> ets:delete(T, internal_key(K)).

-spec first(table()) -> internal_key() | eot().
first(T) -> ets:first(T).

-spec last(table()) -> internal_key() | eot().
last(T) -> ets:last(T).

-spec next(table(), key()) -> internal_key() | eot().
next(T,K) -> ets:next(T, internal_key(K)).

-spec prev(table(), key()) -> internal_key() | eot().
prev(T,K) -> ets:prev(T, internal_key(K)).

-spec first_child(table(), key()) -> internal_key() | eot().
first_child(Table,Parent0) ->
    Parent = internal_key(Parent0),
    case ets:next(Table, Parent) of
	?eot -> ?eot;
	Child ->
	    case is_parent(Parent, Child) of
		{true, FirstChild} -> FirstChild;
		false -> ?eot
	    end
    end.

-spec last_child(table(), key()) -> internal_key() | eot().
last_child(Table,Parent0) ->
    Parent = internal_key(Parent0),
    case ets:prev(Table, Parent++[<<"~">>]) of
	?eot -> ?eot;
	Child ->
	    case is_parent(Parent, Child) of
		{true,LastChild} -> LastChild;
		false -> ?eot
	    end
    end.

-spec next_sibling(table(), key()) -> internal_key() | eot().
next_sibling(Table,Child0) ->
    Child = internal_key(Child0),
    case ets:next(Table,Child++[<<"~">>]) of
	?eot -> ?eot;
	Child2 ->
	    case is_sibling(Child,Child2) of
		{true,Sibling} -> Sibling;
		false -> ?eot
	    end
    end.

-spec prev_sibling(table(), key()) -> internal_key() | eot().
prev_sibling(Table,Child0) ->
    Child = internal_key(Child0),
    case ets:prev(Table,Child) of
	?eot -> ?eot;
	Child2 ->
	    case is_sibling(Child,Child2) of
		{true,Sibling} -> Sibling;
		false -> ?eot
	    end
    end.

%% is_parent is true if Parent is a Prefix of Child
%% [a,b] is a parent of [a,b,c,d]  and {true,[a,b,c]} is returned
is_parent(Parent, Child) ->
    is_parent_(Parent, Child, []).

is_parent_([H|Parent], [H|Child], NewChild) ->
    is_parent_(Parent, Child, [H|NewChild]);
is_parent_([], [H|_Child], NewChild) ->
    {true, lists:reverse(NewChild,[H])};
is_parent_(_, _, _) ->
    false.


%% is_sibling is true if Child1 is a sibling to Child2
%% example: [a,b,c] is a sibling of [a,b,d,e]  and {true,[a,b,d]} is returned
is_sibling(Child1, Child2) ->
    is_sibling_(Child1, Child2, []).

is_sibling_([H|Child1], [H|Child2], Parent) ->
    is_sibling_(Child1, Child2, [H|Parent]);
is_sibling_([H1], [H2|_], Parent) when H1 =/= H2 ->
    {true, lists:reverse(Parent,[H2])};
is_sibling_(_, _, _) ->
    false.

%% depth first traversal
depth_first(Table,Func,Acc) ->
    foldl(Table,Func,Acc).

breadth_first(Table,Func,Acc) ->
    foldbl(Table,Func,Acc).
    

%% Fold over the tree depth first left to right
-spec foldl(table(), function(), term()) -> term().

foldl(Table,Func,Acc) ->
    foldl_(Table,Func,Acc,first(Table)).

foldl_(_Table,_Func,Acc,?eot) ->
    Acc;
foldl_(Table,Func,Acc,K) ->
    [Elem] = lookup(Table,K),
    Acc1 = Func(Elem,Acc),
    foldl_(Table,Func,Acc1,ets:next(Table,K)).

%% Fold over the tree depth first right to left
-spec foldr(table(), function(), term()) -> term().

foldr(Table,Func,Acc) ->
    foldr_(Table,Func,Acc,last(Table)).

foldr_(_Table,_Func,Acc,?eot) ->
    Acc;
foldr_(Table,Func,Acc,K) ->
    [Elem] = lookup(Table,K),
    Acc1 = Func(Elem,Acc),
    foldr_(Table,Func,Acc1,ets:prev(Table,K)).

%% Fold over the tree bredth first left to right
foldbl(Table,Func,Acc) ->
    Q = enql(Table,[],queue:new()),
    foldbl_(Table,Func,Acc,Q).

foldbl_(Table,Func,Acc,Q) ->
    case queue:out(Q) of
	{{value,K},Q1} ->
	    Q2 = enql(Table,K,Q1),	    
	    case lookup(Table,K) of
		[Elem] ->
		    Acc1 = Func(Elem,Acc),
		    foldbl_(Table,Func,Acc1,Q2);
		[] ->
		    foldbl_(Table,Func,Acc,Q2)
	    end;
	{empty,_Q1} ->
	    Acc
    end.

%% Fold over the tree breadth first right to left
foldbr(Table,Func,Acc) ->
    Q = enqr(Table,[],queue:new()),
    foldbr_(Table,Func,Acc,Q).

foldbr_(Table,Func,Acc,Q) ->
    case queue:out(Q) of
	{{value,K},Q1} ->
	    Q2 = enqr(Table,K,Q1),	    
	    case lookup(Table,K) of
		[Elem] ->
		    Acc1 = Func(Elem,Acc),
		    foldbr_(Table,Func,Acc1,Q2);
		[] ->
		    foldbr_(Table,Func,Acc,Q2)
	    end;
	{empty,_Q1} ->
	    Acc
    end.

%% enqueue children left to right
-spec enql(Table::table(),Key::key(),Queue::?QUEUE_T()) ->
		  NewQueue::?QUEUE_T().
enql(Table,K,Q) ->
    enql_(Table,first_child(Table,K), Q).

enql_(_Table,?eot,Q) ->
    Q;
enql_(Table,K,Q) ->
    enql_(Table,next_sibling(Table,K), queue:in(K, Q)).

%% enqueue children right to left
-spec enqr(Table::table(),Key::key(),Queue::?QUEUE_T()) ->
		  NewQueue::?QUEUE_T().
enqr(Table,K,Q) ->
    enqr_(Table,last_child(Table,K), Q).

enqr_(_Table,?eot,Q) ->
    Q;
enqr_(Table,K,Q) ->
    enqr_(Table,prev_sibling(Table,K), queue:in(K, Q)).


%% Set key to it's internal form
to_internal(Pos,Elem) ->
    Key = internal_key(element(Pos, Elem)),
    setelement(Pos,Elem,Key).

%% Set key to it's external form
to_external(Pos,Elem) ->
    Key = external_key(element(Pos, Elem)),
    setelement(Pos,Elem,Key).

%%
%% External keys are in the form 
%% "aaa.bbb.ccc" or <<"aaa.bbb.ccc">> that is
%% key() = <<"path()">> or "path()"
%% path() = ""
%% path() = component() "." path()
%% component() = [a-zA-Z0-9_-]+
%%
%% convert a string or a binary into a internal key
internal_key(Key) when ?is_internal_key(Key) ->
    Key;
internal_key(Name) when is_binary(Name) ->
    binary:split(Name, <<".">>, [global]);
internal_key(Name) when is_list(Name) ->
    internal_key(list_to_binary(Name)).

%% convert to an extern id
external_key(Key) when is_list(Key) ->
    binary_to_list(list_to_binary(join(Key, $.))).

join([],_S) -> [];
join([A],_S) -> [A];
join([A|As],S) -> [A,S|join(As,S)].

%% dump
dump(Table) ->
    dump_(Table,first(Table)).

dump_(_Table, '$end_of_table') ->
    ok;
dump_(Table, Key) ->
    io:format("~p\n", [Key]),
    dump_(Table,next(Table,Key)).
