%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex configuration file loader and validator
%%% @end
%%% Created : 22 Jan 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_config).
-compile(export_all).

scan(Rules) ->
    case scan(Rules, [],[],[]) of
	{ok,{Event,Input,Output}} ->
	    {Event,Input,Output};
	Error ->
	    Error
    end.

scan([{event,ID,{App,Flags},Event} | Rules], E, I, O, Err) ->
    {E1,Err1} = add_event(E, ID, App, Flags, Event, Err),
    scan(Rules, E1, I, O, Err1);
scan([{input,ID,{CanID,SubID,Flags,Output}} | Rules], E, I, O, Err) ->
    {I1,Err1} = add_input(I, ID, CanID, SubID, Flags, Output, Err)
    scan(Rules, E, I1, E, O, Err1);
scan([{output,ID,Flags,Actions} | Rules], E, I, O, Err) ->
    {O1,Err1} = add_output(O, ID, Flags, Actions, Err),
    scan(Rules, E, I, O1, Err1);
scan([R | Rules], E, I, O, Err) ->
    scan(Rules, E, I, O, [{bad_rule,R} | Err]);
scan([], E, I, O, []) ->
    {ok,{E,I,O}};
scan([],_E,_I,_O,Err) ->
    {error,Err}.

add_event(E, ID, App, Flags, Event, Err) when 
      is_integer(ID); is_atom(ID) ->
    case lists:keyfind(ID, #event_rule.id, E) of
	false ->
	    {[#event_rule { id=ID,
			    app=App,
			    flags=Flags,
			    event=Event } | E], Err};
	_ ->
	    {E, [{event_already_exist, ID} | Err]}
    end.




	    

