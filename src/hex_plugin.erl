%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Behaviour for hex_plugin
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_plugin).

-export([behaviour_info/1]).

-include("../include/hex.hrl").

-spec behaviour_info(Arg::callbacks) -> 
			    list({FunctionName::atom(), Arity::integer()}).

behaviour_info(callbacks) ->
    [{add_event, 2}, {del_event, 1}, {output, 2}];
behaviour_info(_) ->
    undefined.
%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%

%%
%%  del_event(Ref::reference()) ->
%%     ok.

%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
