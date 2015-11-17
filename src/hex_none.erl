%%% coding: latin-1
%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2015, Rogvall Invest AB, <tony@rogvall.se>
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
%%%-------------------------------------------------------------------
%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Dummy hex output module
%%%
%%% Created : November 2015 by Malotte W Lönne
%%% @end
%%%-------------------------------------------------------------------
-module(hex_none).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal(), Cb::function()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(_Flags, _Signal, _Cb) ->
    {ok, make_ref()}.

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(_Ref) ->
    ok.

%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    lager:debug("flags=~w, env=~w\n", [Flags, Env]),
    ok.

%%
%% init_event(in | out, Flags::[{atom(),term()}])
%%
init_event(_, _) ->
    ok.

%%
%% mod_event(in | out, Flags::[{atom(),term()}])
%%
mod_event(_, _) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(_Dir, _Flags) ->
    ok.

%%
%% return event specification in internal YANG format
%% {Type,Value,Stmts}
%%
event_spec(_Dir) ->
    [].
