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


