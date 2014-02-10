%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex configuration file loader and validator
%%% @end
%%% Created : 22 Jan 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_config).
-compile(export_all).

-include("../include/hex.hrl").


scan(Rules) ->
    case scan_(Rules, [],[],[], []) of
	{ok,{Event,Input,Output}} ->
	    {Event,Input,Output};
	Error ->
	    Error
    end.

scan_([R | Rs], E, I, O, Err) ->
    case R of
	{event,Label,{App,Flags},Event} when is_atom(App) ->
	    {E1,Err1} = add_event(E,Label,App,Flags,Event,Err),
	    scan_(Rs, E1, I, O, Err1);
	{input,Label,{ID,Chan,Type,Value,Output}} ->
	    {I1,Err1} = add_input(I,Label,ID,Chan,Type,Value,Output, Err),
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
	    %% FIXME: transform Sig into internal form,
	    %% check plugin and let plugin check flags
	    Event =
		#hex_event { label=Label,
			     app=App,
			     flags=Flags,
			     signal=Sig },
	    {[Event | E], Err};
	_ ->
	    {E, [{event_already_exist, Label} | Err]}
    end.


add_input(I, Label, ID, Chan, Type, Value, Output, Err) ->
    case lists:keyfind(Label, #hex_rule.label, I) of
	false ->
	    %% FIXME: check and scan all pattern forms
	    Input = 
		#hex_rule { label = Label,
			    id = ID,
			    chan = Chan,
			    type = Type,
			    value = Value,
			    output = Output },
	    { [Input|I], Err };
	_ ->
	    {I, [{rule_already_exist, Label} | Err]}
    end.
	    
add_output(O,Label,Flags,Actions,Err) ->
    case lists:keyfind(Label, #hex_output.label, O) of
	false ->
	    Output =
		#hex_output { label = Label,
			      flags = Flags,
			      actions = Actions 
			    },
	    { [Output|O], Err };
	_ ->
	    {O, [{output_already_exist, Label} | Err]}
    end.
