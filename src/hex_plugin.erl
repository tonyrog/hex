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
%%%    Behaviour for hex_plugin
%%% @end
%%% Created :  6 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(hex_plugin).

-export([behaviour_info/1]).

-include("../include/hex.hrl").

-spec behaviour_info(Arg::callbacks) -> 
			    list({FunctionName::atom(), Arity::integer()}).

behaviour_info(callbacks) ->
    [{validate_event,2}, {init_event, 2},
     {add_event, 2}, {del_event, 1}, {output, 2}];
behaviour_info(_) ->
    undefined.

%%
%% validate_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,_}
%%

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,_}
%%

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
