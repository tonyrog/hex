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

-export([start/0, debug/0]).
-export([start_all/1]).
-export([auto_join/1, join_async/1, join/1]).
-export([validate_flags/2]).
-export([text_expand/2]).
-export([make_self/1]).
-export([is_string/1]).
-export([pp_yang/1]).
-export([save_yang/2]).
-export([subscribe/0, subscribe/1, unsubscribe/0]).
-export([inform/2]).
-export([event/1, event/2]).
-export([event_and_transmit/2]).
-export([analog_event_and_transmit/2]).
-export([event_list/0]).
-export([event_signal/1]).
-export([signal/5]).
-export([set_signal_value/2]).
-export([input_active/2]).
-export([input2output/1]).
-export([output2pid/1]).
-export([input2output_pid/1]).

-include("../include/hex.hrl").

debug() ->
    start_all(hex),
    start_all(ale),
    ale:debug_gl(hex_output).


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
%% Subscribe to alarms and output_active signals received by hex.
%%
subscribe(Options) ->
    hex_server:subscribe(Options).

subscribe() ->
    hex_server:subscribe([]).

unsubscribe() ->
    hex_server:unsubscribe().

inform(Type, Options) ->
    hex_server:inform(Type, Options).

%%
%% Event access
%%

%% Get all defined events (inventory)
event_list() ->
    hex_server:event_list().

%% Get the signal belonging to event with label Label
event_signal(Label) ->
    hex_server:event_signal(Label).

%% Send an event to hex
event(Signal) ->
    hex_server:event(Signal,[]).
event(Signal, Env) ->
    hex_server:event(Signal, Env).

%% Send an event that will also be transmitted to the can bus
event_and_transmit(Label, Value) ->
    hex_server:event_and_transmit(Label, Value).

%% Send an analog event that will also be transmitted to the can bus
analog_event_and_transmit(Label, Value) ->
    hex_server:analog_event_and_transmit(Label, Value).


%%
%% Input/Output access
%%
input_active(Label, TrueOrFalse) when is_atom(Label), is_boolean(TrueOrFalse) ->
    hex_server:input_active(Label, TrueOrFalse).

input2output(Label) when is_atom(Label) ->
    hex_server:input2output(Label).

output2pid(Channel) when is_integer(Channel) ->
    hex_server:output2pid(Channel).

input2output_pid(Label) when is_atom(Label) ->
    hex_server:input2output_pid(Label).

%%
%% Signal encapsulation
%%
signal(Id, Channel, Type, Value, Source) ->
    #hex_signal {id = Id,
		 chan = Channel,
		 type = Type,
		 value = Value,
		 source = Source}.

set_signal_value(Signal, Value) ->
    Signal#hex_signal{value = Value}.

%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} where var is a encoded atom
%% operating system enviroment is accessed through $(VAR)
%% and application library dir $/app/
%%
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], env, Acc, Env);
text_expand_([$$,$(|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$(,$$], shell, Acc, Env);
text_expand_([$$,$/|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$/,$$], lib, Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).


text_expand_collect_([$)|Text], Var, _Pre, shell, Acc, Env) ->
    case os:getenv(rev_variable(Var)) of
	false ->
	    text_expand_(Text, Acc, Env);
	Value ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([$/|Text], Var, _Pre, lib, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	App ->
	    case code:lib_dir(App) of
		{error,_} ->
		    text_expand_(Text, Acc, Env);
		Value ->
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([$}|Text], Var, _Pre, env, Acc, Env) ->
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
text_expand_collect_([C|Text], Var, Pre, Shell, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C =:= $_; C =:= $@;
       C =:= $\s; C =:= $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Shell, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, _Shell, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).

trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.

%%
%% Utility to create a signal id 
%%
make_self(NodeID) ->
    if NodeID band ?HEX_COBID_EXT =/= 0;
       NodeID > 127 ->
	    %% extended nodeid
	    ?HEX_COBID_EXT bor 
		(2#0011 bsl 25) bor 
		(NodeID band ?HEX_XNODE_ID_MASK);
       true ->
	    %% short nodeid
	    (2#0011 bsl 9) bor (NodeID band 16#7f)
    end.


%%
%% Library function for option validation:
%% Spec is internal YANG format.
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
	{ok,Spec1} -> validate_flags_(Options, Spec++Spec1, Error);
	E -> validate_flags_(Options, Spec, [E|Error])
    end;
validate_flags_([], _Spec, []) ->
    ok;
validate_flags_([], _Spec, Error) ->
    {error, Error}.


validate_spec_([{leaf,Key,As}|Spec],Options,Error) ->
    case lists:keymember(Key,1,Options) of
	false ->
	    case lists:keyfind(mandatory,1,As) of
		false ->
		    validate_spec_(Spec, Options, Error);
		{mandatory,false,_} ->
		    validate_spec_(Spec, Options, Error);
		{mandatory,true,_} ->
		    validate_spec_(Spec, Options, [{mandatory,Key}|Error])
	    end;
	_ ->
	    validate_spec_(Spec, Options, Error)
    end;
validate_spec_([_|Spec],Options, Error) -> %% fixme: check container
    validate_spec_(Spec, Options, Error);
validate_spec_([], _Options, []) ->
    ok;
validate_spec_([], _Options, Error) ->
    {error,Error}.


validate_flag({Key,Value}, Spec) ->
    case lists:keyfind(Key, 2, Spec) of
	{leaf, Key, Stmts } ->
	    validate_leaf(Key, Value, Stmts);
	{'leaf-list', Key, Stmts } ->
	    validate_leaf(Key, Value, Stmts);
	{container, Key, Stmts} ->
	    validate_flags(Value, Stmts);
	{list, Key, Stmts} ->
	    validate_list(Key, Value, Stmts);
	{choice, Key, Stmts} ->
	    validate_choice(Key, Value, Stmts);
	false ->
	    {unknown,Key}
    end;
validate_flag(_Value, _Spec) ->
    {error,badarg}.


validate_choice(Key, Value, Stmts) ->
    lager:debug("validate_choice: ~p, ~p, ~p", [Key, Value, Stmts]),
    case Value of
	false -> {error,{Key,badarg}};
	{Case,CaseValue} ->
	    case lists:keyfind(Case,2,Stmts) of
		{'case',_,CaseStmts} ->
		    case lists:keyfind(Case, 2, CaseStmts) of
			false -> {error,{Key,{missing_choice,Case}}};
			{leaf,Case,Stmts1} ->
			    validate_leaf(Case, CaseValue, Stmts1);
			{_,_,Stmts1} ->
			    validate_flags(CaseValue, Stmts1)
		    end
	    end
    end.

validate_leaf(Key, Value, Stmts) ->
    case lists:keyfind(type, 1, Stmts) of
	false ->
	    {missing_type, Key};
	{type, Type, Tas} ->
	    case validate_value(Value, Type, Tas) of
		ok -> ok;
		{error,Error} -> {error,{Key,Error}};
		Error -> {error, {Key,Error}}
	    end
    end.

validate_list(_Key, Value, Stmts) ->
    case lists:keytake(key, 1, Stmts) of
	false ->
	    {error, {missing,key}};
	{value,{key,ListKey,_},Stmts1} ->
	    case lists:keyfind(ListKey, 2, Stmts1) of
		{leaf, ListKey, _} ->
		    validate_flags(Value, Stmts1);
		{container, ListKey, Stmts2} ->
		    lists:foldl(
		      fun({K,V}, Acc) when K =:= ListKey ->
			      case validate_flags(V, Stmts2)of
				  ok -> ok;
				  E -> [E | Acc]
			      end
		      end, [], Value);
		{choice, ListKey, Stmts2} ->
		    lists:foldl(
		      fun(V, Acc) ->
			      case validate_choice(ListKey, V, Stmts2) of
				  ok -> ok;
				  E -> [E | Acc]
			      end
		      end, [], Value);
		_W ->
		    %% io:format("~p\n", [W]),
		    {error, {missing,ListKey}}
	    end
    end.





-define(r(Min,Max), {range,[{(Min),(Max)}],[]}).

validate_value(Value, boolean, Tas) ->
    if is_boolean(Value) -> restrict_value(Value, boolean, Tas);
       is_atom(Value) -> badarg;
       true ->
	    {range,[true,false]}
    end;
validate_value(Value, uint8, Tas) ->
    validate_uint(Value, uint8, [?r(0,16#ff)|Tas]);
validate_value(Value, uint16, Tas) ->
    validate_uint(Value, uint16, [?r(0,16#ffff)|Tas]);
validate_value(Value, uint32, Tas) ->
    validate_uint(Value, uint32, [?r(0,16#ffffffff)|Tas]);
validate_value(Value, uint64, Tas) ->
    validate_uint(Value, uint64, [?r(0,16#ffffffffffffffff)|Tas]);
validate_value(Value, int8, Tas) ->
    validate_int(Value,int8,[?r(-16#80,16#7f)|Tas]);
validate_value(Value, int16, Tas) ->
    validate_int(Value,int16,[?r(-16#8000,16#7fff)|Tas]);
validate_value(Value, int32, Tas) ->
    validate_int(Value,int32,[?r(-16#80000000,16#7fffffff)|Tas]);
validate_value(Value, int64, Tas) ->
    validate_int(Value,int64,
		   [?r(-16#8000000000000000,16#7fffffffffffffff)|Tas]);
validate_value(Value, decimal64, Tas) ->
    if is_float(Value) ->
	    restrict_value(Value, decimal64, Tas);
       %% maybe accept integer encoding here as well?
       true ->
	    badarg
    end;
validate_value(Value, enumeration, Tas) ->
    if is_atom(Value) ->
	    validate_enum(Value, Tas);
       true ->
	    Es = [E || {enum,E,_} <- Tas],
	    {enumeration, Es}
    end;
validate_value(Value, bits, Tas) ->
    if is_atom(Value) -> validate_bit(Value, Tas);
       Value =:= [] -> ok;
       is_list(Value) ->
	    case lists:all(fun(V) -> validate_bit(V, Tas) =:= ok end, Value) of
		true -> ok;
		false -> error_bits(Tas)
	    end;
       true -> error_bits(Tas)
    end;
validate_value(Value, string, Tas) ->
    case is_atom(Value) orelse is_string(Value) of
	true ->  restrict_value(Value, string, Tas);
	false -> badarg
    end;
validate_value(Value, binary, Tas) ->
    case is_iolist(Value) of
	true -> restrict_value(Value, binary, Tas);
	false -> badarg
    end;
validate_value(Value, union, Tas) ->
    case lists:any(fun({type,Type,Tas1}) ->
			   case validate_value(Value, Type, Tas1) of
			       ok -> true;
			       _ -> false
			   end;
		      (_) -> false
		   end, Tas) of
	true -> ok;
	false -> badarg
    end;
validate_value(Value, anyxml, _Tas) ->
    if is_list(Value) ->
	    ok;
       true -> badarg
    end;
validate_value(_Value, 'hex:rfc822', _Tas) ->  %% fixme!
    %% validate email address
    ok;
validate_value(Value, 'yang:ip-address', _Tas) when is_tuple(Value)->
    case inet:ntoa(Value) of
	Address when is_list(Address) -> ok;
	{error, einval} -> badarg
    end;
validate_value(Value, 'yang:ip-address', _Tas) when is_list(Value)->
    case inet_parse:address(Value) of
	{ok, _Address} -> ok;
	{error, einval} -> badarg
    end;
validate_value(Value, 'yang:domain-name',_Tas) -> %% fixme!
    try inet_parse:domain(Value) of
	true -> ok;
	false -> badarg
    catch
	error:_ -> badarg
    end;
validate_value(Value, 'yang:port-number',_Tas) -> %% fixme!
    if is_integer(Value), Value >= 0, Value =< 65535 -> ok;
       true -> badarg
    end.

validate_uint(Value, Type, Tas) ->
    if is_integer(Value), Value >= 0 -> restrict_value(Value, Type, Tas);
       true -> badarg
    end.

validate_int(Value, Type, Tas) ->
    if is_integer(Value) -> restrict_value(Value, Type, Tas);
       true -> badarg
    end.

restrict_value(Value, Type,[{range,Range,_}|Opts]) ->
    case lists:any(fun({min,max}) -> true;
		      ({min,Max}) -> Value =< Max;
		      ({Min,max}) -> Value >= Min;
		      ({Min,Max}) -> (Value >= Min) andalso (Value =< Max);
		      (V) -> Value =:= V
		   end, Range) of
	true ->
	    restrict_value(Value, Type,Opts);
	false ->
	    {range,Range}
    end;
restrict_value(Value, Type, [{length,Range,_}|Opts]) ->
    Len = if Type =:= string -> length(Value);
	     Type =:= binary -> erlang:iolist_size(Value)
	  end,
    case lists:any(fun({min,max}) -> true;
		      ({min,Max}) -> Len =< Max;
		      ({Min,max}) -> Len >= Min;
		      ({Min,Max}) -> (Len >= Min) andalso (Len =< Max);
		      (L) -> Len =:= L
		   end, Range) of
	true ->
	    restrict_value(Value, Type, Opts);
	false ->
	    {length,Range}
    end;
restrict_value(Value, Type, [{pattern,RegExp,_}|Opts]) ->
    case xsdre:match(Value, RegExp) of
	true ->
	    restrict_value(Type, Value, Opts);
	false ->
	    {pattern, RegExp}
    end;
restrict_value( _Value, _Type, []) ->
    ok.

validate_enum(Value, [{enum,Name,_As}|List]) ->
    if Value =:= Name -> ok;
       true -> validate_enum(Value, List)
    end;
validate_enum(Value, [_|List]) ->
    validate_enum(Value, List);
validate_enum(_Value, []) ->
    badarg.

validate_bit(Value, [{bit,Name,_As}|List]) ->
    if Value =:= Name -> ok;
       true -> validate_bit(Value, List)
    end;
validate_bit(Value, [_|List]) ->
    validate_bit(Value, List);
validate_bit(_Value, []) ->
    badarg.

error_bits(Tas) ->
    {bits, [E || {bit,E,_} <- Tas]}.

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

%% save yang spec to file
save_yang(File, App) ->
    file:write_file(File, list_to_binary(pp_yang(App))).

%% pretty format a spec as yang container
pp_yang(hex_config) ->
    Conf = hex_config:config_spec(),
    Yang = {module, hex_config,
	    [{namespace, "http://rogvall.se/hex", []},
	     {prefix, "hex", []},
	     Conf]},
    pp_yang(Yang);

pp_yang(hex_input) ->
    Conf = hex_input:event_spec(),
    Yang = {module, hex_input,
	    [{namespace, "http://rogvall.se/hex", []},
	     {prefix, "hex", []},
	     {container, config, Conf}]},
    pp_yang(Yang);
pp_yang(hex_output) ->
    Conf = hex_output:event_spec(),
    Yang = {module, hex_output,
	    [{namespace, "http://rogvall.se/hex", []},
	     {prefix, "hex", []},
	     {container, config, Conf}]},
    pp_yang(Yang);
pp_yang(App) when is_atom(App) ->
    AppName = atom_to_list(App),
    Conf = yang_spec(App),
    Yang = {module, App,
	    [{namespace, "http://rogvall.se/hex/"++AppName, []},
	     {prefix, AppName, []},
	     {container, config, [Conf]}]},
    pp_yang(Yang);
pp_yang(Yang) when is_tuple(Yang) ->
    pp_yang("", Yang);
pp_yang(Yang) when is_list(Yang) ->
    pp_yang_list("", Yang).

pp_yang(Ident, {Tag,Value,Sub}) ->
    ValueString = pp_yang_value(Tag, Value, Sub),
    [Ident,atom_to_list(Tag)," ", ValueString,
     pp_yang_list(Ident, Sub)].

pp_yang_list(_Ident,[]) -> ";\n";
pp_yang_list(Ident,Spec) ->
    [" {\n", [ pp_yang(["  ",Ident], S) || S <- Spec ], Ident, "}\n"].

yang_spec(App) ->
    {container,App,
     [{container,list_to_atom(atom_to_list(App)++"_in"),
       App:event_spec(in)},
      {container,list_to_atom(atom_to_list(App)++"_out"),
       App:event_spec(out)}
     ]}.

pp_yang_value(range, Value, _Sub) ->
    pp_yang_range(Value);
pp_yang_value(length, Value, _Sub) ->
    pp_yang_range(Value);
pp_yang_value(_, Value, _Sub) ->
    io_lib:format("~p", [Value]).

pp_yang_range_elem({Min,Max}) -> io_lib:format("~w..~w", [Min,Max]);
pp_yang_range_elem(Value) ->io_lib:format("~w", [Value]).

pp_yang_range([E]) -> pp_yang_range_elem(E);
pp_yang_range([E|Es]) -> [pp_yang_range_elem(E),"|",pp_yang_range(Es)];
pp_yang_range([]) -> "".

