%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Start
%%% @end
%%% Created : 26 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex).

-export([start/0]).
-export([start_all/1]).
-export([auto_join/1, join_async/1, join/1]).
-export([validate_flags/2]).
-export([text_expand/2]).

-include("../include/hex.hrl").


start() ->
    start_all(hex).

%% utility since application:ensure_all_started is not present in R15
%% this must be used for now.
start_all(App) when is_atom(App) ->
    each_application_([App], []);
start_all(Apps) when is_list(Apps) ->
    each_application_(Apps, []).

each_application_([App|Apps], Started) ->
    case application:start(App) of
	{error,{not_started,App1}} ->
	    each_application_([App1,App|Apps],Started);
	{error,{already_started,App}} ->
	    each_application_(Apps,Started);
	ok ->
	    each_application_(Apps,[App|Started]);
	Error ->
	    Error
    end;
each_application_([], Started) ->
    {ok, Started}.

%%
%% Plugins normally calls join from the server init functions
%% to register as a live plugin 
%%
auto_join(Name) when is_atom(Name) ->
    case application:get_all_env(hex) of
	undefined ->  %% application is not loaded nor started
	    false;
	Env when is_list(Env) ->  %% hex is at least loaded
	    AutoJoin = case application:get_env(hex, auto_join) of
			   {ok,AutoJ} when is_boolean(AutoJ) -> AutoJ;
			   undefined -> true  %% default
		       end,
	    if AutoJoin ->
		    case whereis(hex_server) of
			undefined -> false;
			Pid when is_pid(Pid) ->
			    join_async(Name)
		    end;
	       true ->
		    false
	    end
    end.

join_async(Name) when is_atom(Name) ->
    gen_server:cast(hex_server, {join,self(),Name}).

join(Name) when is_atom(Name) ->
    gen_server:call(hex_server, {join,self(),Name}).

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} wher var is a encoded atom
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).

text_expand_collect_([$}|Text], Var, _Pre, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	Key ->
	    case lists:keyfind(Key, 1, Env) of
		false -> 
		    text_expand_(Text, Acc, Env);
		{_,Val} ->
		    Value = lists:flatten(io_lib:format("~w", [Val])),
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C == $_; C == $@;
       C == $\s; C == $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).
    
trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.

%%
%% Library function for option validation:
%% Spec = [{Key,optional|mandatory,Type,Default}]
%% Type =  integer|string(unicode)|iolist|binary|bitstring|boolean|bool|
%%         atom|timeout|{tuple,[Type]}|{list,Type},{record,atom()}
%%         unsigned|unsigned1|unsigned8|unsigned16|unsigned32|unsigned64|
%%         integer|integer8|integer16|integer32|integer64|
%%         float|float01|number|
%%         {integer,Min,Max}|{number,Min,Max}|{float,Min,Max}|
%%         {sys_config,App::atom(),Spec}|
%%         fun (Value) -> boolean()
%%
%% return ok | {error, Reason}
%% Reason:
%%   [ {mandatory,Key} |
%%     {unknown,Key} |
%%     {missing_sys_config,{Key,Value}} |
%%     {range,{Key,Range}} |
%%     {badarg,Key} ]
%%
validate_flags(Options, Spec) ->
    case validate_flags_(Options, Spec, []) of 
	ok ->
	    validate_spec_(Spec, Options, []);
	{error,Error} ->
	    validate_spec_(Spec, Options, Error)
    end.

validate_flags_([Option|Options], Spec, Error) ->
    case validate_flag(Option, Spec) of
	ok -> validate_flags_(Options, Spec, Error);
	E -> validate_flags_(Options, Spec, [E|Error])
    end;
validate_flags_([], _Spec, []) ->
    ok;
validate_flags_([], _Spec, Error) ->
    {error, Error}.

validate_flag({Key,Value}, Spec) ->
    case lists:keyfind(Key, 1, Spec) of
	false -> 
	    {unknown,Key};
	{_Key,_Status,Type,_} ->
	    case validate_value(Value, Type) of
		ok -> 
		    ok;
		badarg ->
		    {badarg,Key};
		{range,Range} -> 
		    {range,{Key,Range}};
		{missing_sys_config,{App,Value}} ->
		    {missing_sys_config,{Key,{App,Value}}};
		{error,List}-> %% from recursive (sys config) validation
		    {badarg, {Key,List}}
	    end
    end.

validate_spec_([{Key,mandatory,_,_}|Spec],Options,Error) ->
    case lists:keymember(Key,1,Options) of
	false ->
	    validate_spec_(Spec, Options, [{mandatory,Key}|Error]);
	true ->
	    validate_spec_(Spec, Options, Error)
    end;
validate_spec_([{_,optional,_,_}|Spec],Options, Error) ->
    validate_spec_(Spec, Options, Error);
validate_spec_([], _Options, []) ->
    ok;
validate_spec_([], _Options, Error) ->
    {error,Error}.


%% validate a value
%% return ok | badarg | {range,ValidRange}
validate_value(Value, boolean) ->
    if is_boolean(Value) -> ok;
       is_atom(Value) -> badarg;
       true -> {range,[true,false]}
    end;
validate_value(Value, bool) ->
    if is_boolean(Value); ?is_uint1(Value) ->  ok;
       true -> {range, [true,false,{0,1}]}
    end;
validate_value(Value, {const,Const}) ->
    if Value =:= Const -> ok;
       true -> badarg
    end;
validate_value(Value, timeout) ->
    validate_value(Value, {alt,[unsigned32|{const,infinity}]});
validate_value(Value, unsigned1) ->
    validate_integer_range(Value, 0, 1);
validate_value(Value, unsigned8) ->
    validate_integer_range(Value, 0, 16#ff);
validate_value(Value, unsigned16) ->
    validate_integer_range(Value, 0, 16#ffff);
validate_value(Value, unsigned32) ->
    validate_integer_range(Value, 0, 16#ffffffff);
validate_value(Value, unsigned64) ->
    validate_integer_range(Value, 0, 16#ffffffffffffffff);
validate_value(Value, unsigned) ->
    if is_integer(Value) ->
	    if Value >= 0 -> ok;
	       true -> {range, [{0,'..'}]}
	    end;
       true -> badarg
    end;
validate_value(Value, integer8) ->
    validate_integer_range(Value, -16#80, 16#7f);
validate_value(Value, integer16) ->
    validate_integer_range(Value, -16#8000, 16#7fff);
validate_value(Value, integer32) ->
    validate_integer_range(Value,-16#80000000, 16#7fffffff);
validate_value(Value, integer64) ->
    validate_integer_range(Value,-16#8000000000000000,16#7fffffffffffffff);
validate_value(Value, integer) ->
    if is_integer(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, {integer,Min,Max}) 
  when is_integer(Min), is_integer(Max), Min =< Max ->
    validate_integer_range(Value, Min, Max);
validate_value(Value, number) ->
    if is_number(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, {number,Min,Max})
  when is_number(Min), is_number(Max), Min =< Max ->
    validate_number_range(Value, Min, Max);
validate_value(Value, float) ->
    if is_float(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, {float,Min,Max})
  when is_float(Min), is_float(Max), Min =< Max ->
    validate_float_range(Value, Min, Max);
validate_value(Value, float01) ->
    if is_number(Value), Value >= 0.0, Value=< 1.0 -> ok;
       is_number(Value) -> {range, [{0.0,1.0}]};
       true -> badarg
    end;
validate_value(Value, atom) ->
    if is_atom(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, string) ->
    case is_string(Value) of
	true  -> ok;
	false -> badarg
    end;
validate_value(Value, iolist) ->
    case is_iolist(Value) of
	true -> ok;
	false -> badarg
    end;
validate_value(Value, binary) ->
    if is_binary(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, bitstring) ->
    if is_bitstring(Value) -> ok;
       true -> badarg
    end;
validate_value(Value, {alt,Ts}) when is_list(Ts) ->
    validate_value_alt(Value, Ts);
validate_value(Value, {tuple,Ts}) when is_list(Ts) ->
    N = length(Ts),
    if is_tuple(Value), tuple_size(Value) =:= N ->
	    validate_tuple_elems(tuple_to_list(Value), Ts);
       true ->
	    badarg
    end;
validate_value(Value, {record,Name}) when is_atom(Name) ->
    if is_tuple(Value), element(1,Value) =:= Name ->
	    ok;
       true ->
	    badarg
    end;
validate_value(Value, {list,T}) ->
    if is_list(Value) ->
	    validate_list_elems(Value, T);
       true ->
	    badarg
    end;
validate_value(Value, {sys_config,App,Spec}) 
  when is_atom(App), is_list(Spec) ->
    if is_atom(Value) ->
	    case application:get_env(App,Value) of
		{ok,ConfValue} ->
		    validate_flags(ConfValue,Spec);
		_ ->
		    {missing_sys_config,{App,Value}}
	    end;
       true ->
	    badarg
    end;
validate_value(Value, Func) when is_function(Func) ->
    try Func(Value) of
	true -> ok;
	false -> badarg
    catch
	error:_ ->
	    badarg
    end.

validate_integer_range(Value, Min, Max) ->
    if is_integer(Value) -> validate_range(Value, Min, Max);
       true -> badarg
    end.

validate_number_range(Value, Min, Max) ->
    if is_number(Value) -> validate_range(Value, Min, Max);
       true -> badarg
    end.

validate_float_range(Value, Min, Max) ->
    if is_float(Value) -> validate_range(Value, Min, Max);
       true -> badarg
    end.

validate_range(Value, Min, Max) when Value >= Min, Value =< Max ->
    ok;
validate_range(_Value, Min, Max) ->
    {range,[{Min,Max}]}.


%% check value with alternate type spec
validate_value_alt(Value, [T|Ts]) ->
    case validate_value(Value, T) of
	ok -> ok;
	_Error ->
	    validate_value_alt(Value,Ts)
    end;
validate_value_alt(_Value, []) ->
    badarg.


%% validate tuple elements
validate_tuple_elems([Value|Vs], [T|Ts]) ->
    case validate_value(Value, T) of
	ok ->
	    validate_tuple_elems(Vs,Ts);
	Error ->
	    Error
    end;
validate_tuple_elems([], []) ->
    ok.

%% check list/array element types
validate_list_elems([Value|Vs], T) ->
    case validate_value(Value, T) of
	ok -> 
	    validate_list_elems(Vs, T);
	Error ->
	    Error
    end;
validate_list_elems([], _) ->
    ok.

is_string(Value) ->
    try unicode:characters_to_binary(Value) of
	Utf8 when is_binary(Utf8) -> true;
	{error,_,_} -> false
    catch
	error:_ -> false
    end.

is_iolist(Value) ->
    try (erlang:iolist_size(Value) >= 0) of
	Bool -> Bool
    catch
	error:_ -> false
    end.
