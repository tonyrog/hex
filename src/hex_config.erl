%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex configuration file loader and validator
%%% @end
%%% Created : 22 Jan 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_config).
-compile(export_all).

-include("../include/hex.hrl").

%% COB function codes (from canopen.hrl)
-define(NMT,               2#0000).
-define(SYNC,              2#0001).
-define(TIME_STAMP,        2#0010).
-define(PDO1_TX,           2#0011).
-define(PDO1_RX,           2#0100).
-define(PDO2_TX,           2#0101).
-define(PDO2_RX,           2#0110).
-define(PDO3_TX,           2#0111).
-define(PDO3_RX,           2#1000).
-define(PDO4_TX,           2#1001).
-define(PDO4_RX,           2#1010).
-define(SDO_TX,            2#1011).
-define(SDO_RX,            2#1100).
-define(NODE_GUARD,        2#1110).
-define(LSS,               2#1111).
-define(EMERGENCY,         2#0001).

-define(COBID_ENTRY_EXTENDED,       16#20000000).

scan(Rules) ->
    scan_(Rules, [],[],[], []).


scan_([R | Rs], E, I, O, Err) ->
    case R of
	{event,Label,{App,Flags},Event} when is_atom(App) ->
	    {E1,Err1} = add_event(E,Label,App,Flags,Event,Err),
	    scan_(Rs, E1, I, O, Err1);
	{input,Label,Event,Flags,Output} ->
	    {I1,Err1} = add_input(I,Label,Event,Flags,Output,Err),
	    scan_(Rs, E, I1, O, Err1);
	{output,Label,Flags,Actions} ->
	    {O1,Err1} = add_output(O,Label,Flags,Actions,Err),
	    scan_(Rs, E, I, O1, Err1);
	_ ->
	    scan_(Rs, E, I, O, [{bad_rule,R} | Err])
    end;
scan_([], E, I, O, []) ->
    {ok,{E,I,O}};
scan_([],_E,_I,_O,Err) ->
    {error,Err}.

add_event(E, Label, App, Flags, Sig, Err) ->
    case lists:keyfind(Label, #hex_event.label, E) of
	false ->
	    Err1 = case validate_event(App, in, Flags) of
		       ok -> Err;
		       Error1 -> [{event,Label,Error1}|Err]
		   end,
	    case hex_pattern(Sig) of
		{ok,Pattern} ->
		    Event =
			#hex_event { label=Label,
				     app=App,
				     flags=Flags,
				     signal=Pattern },
		    {[Event|E],Err1};
		Error2 ->
		    {E, [{event,Label,Error2}|Err1]}
	    end;
	_ ->
	    {E, [{event,Label,{error,ealready}} | Err]}
    end.


add_input(I, Label, Sig, Flags, Output, Err) ->
    case lists:keyfind(Label, #hex_input.label, I) of
	false ->
	    Err1 = 
		case hex_input:validate_flags(Flags) of
		    ok -> Err;
		    Error1 -> [{input,Label,Error1}|Err]
		end,
	    case hex_pattern(Sig) of
		{ok,Pattern} ->
		    Input = 
			#hex_input { label = Label,
				     signal = Pattern,
				     flags = Flags,
				     output = Output },
		    { [Input|I], Err1 };
		Error ->
		    {I, [{input,Label,Error}|Err1]}
	    end;
	_ ->
	    {I, [{input,Label,{error,ealready}} | Err]}
    end.
	    
add_output(O,Label,Flags,Actions,Err) ->
    case lists:keyfind(Label, #hex_output.label, O) of
	false ->
	    Err1 = 
		case hex_output:validate_flags(Flags) of
		    ok -> Err;
		    Error1 -> [{output,Label,Error1}|Err]
		end,
	    Err2 = case validate_actions(Actions) of
		       ok -> Err1;
		       Error2 -> [{event,Label,Error2}|Err1]
		   end,
	    Output =
		#hex_output { label = Label,
			      flags = Flags,
			      actions = Actions 
			    },
	    { [Output|O], Err2 };
	_ ->
	    {O, [{output,Label,{error,ealready}} | Err]}
    end.

%% translate event pattern into internal form
hex_pattern({ID,Chan,Type,Value}) ->
    try #hex_pattern { id = pattern(ID),
		       chan = pattern(Chan),
		       type = pattern(Type),
		       value = pattern(Value) } of
	P=#hex_pattern{} -> {ok, P}
    catch
	error:_ -> {error,bad_pattern}
    end.

pattern(P) when ?is_uint32(P) -> P;
pattern(P={mask,Mask,Match}) when ?is_uint32(Mask),?is_uint32(Match) -> P;
pattern(P={range,Low,High}) when is_integer(Low), is_integer(High) -> P;
pattern({'not',P1}) -> {'not',pattern(P1)};
pattern({'and',P1,P2}) -> {'and',pattern(P1),pattern(P2)};
pattern({'or',P1,P2}) -> {'or',pattern(P1),pattern(P2)};
pattern(digital) -> ?HEX_DIGITAL;
pattern(analog) -> ?HEX_ANALOG;
pattern(encoder) -> ?HEX_ENCODER;
pattern(rfid) -> ?HEX_RFID;
pattern(Var) when is_atom(Var) -> Var;
pattern({xcobid,Func,ID}) when ?is_uint25(ID) ->
    ?COBID_ENTRY_EXTENDED + (func(Func) bsl 25) + ID;
pattern({xcobid,ID}) when ?is_uint29(ID) ->
    ?COBID_ENTRY_EXTENDED + ID;
pattern({cobid,Func,ID}) when ?is_uint7(ID) ->
    (func(Func) bsl 7) + ID;
pattern({cobid,ID}) when ?is_uint11(ID) ->
    ID;
pattern([P|Ps]) -> [pattern(P)|pattern(Ps)];
pattern([]) -> [].

%% translate function part of COB    
func(nmt) -> ?NMT;
func(sync) ->?SYNC;
func(time_stamp) -> ?TIME_STAMP;
func(pdo1_tx) -> ?PDO1_TX;
func(pdo1_rx) -> ?PDO1_RX;
func(pdo2_tx) -> ?PDO2_TX;
func(pdo2_rx) -> ?PDO2_RX;
func(pdo3_tx) -> ?PDO3_TX;
func(pdo3_rx) -> ?PDO3_RX;
func(pdo4_tx) -> ?PDO4_TX;
func(pdo4_rx) -> ?PDO4_RX;
func(sdo_tx) -> ?SDO_TX;
func(sdo_rx) -> ?SDO_RX;
func(node_guard) -> ?NODE_GUARD;
func(lss) -> ?LSS;
func(emergency) -> ?EMERGENCY;
func(F) when ?is_uint4(F) -> F.


validate_event(App, Dir, Flags) when is_atom(App) ->
    case application:load(App) of
	ok ->
	    App:validate_event(Dir, Flags);
	{error,{already_loaded,App}} ->
	    App:validate_event(Dir, Flags);
	Error ->
	    Error
    end.

validate_actions({App,Flags}) ->
    validate_event(App, out, Flags);
validate_actions([{_Pattern,{App,Flags}} | Actions]) ->
    %% fixme: validate pattern!
    case validate_event(App, out, Flags) of
	ok -> validate_actions(Actions);
	Error -> Error
    end;
validate_actions([]) ->
    ok.
