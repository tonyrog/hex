-module(hex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Opts = case application:get_env(hex, options) of
	       undefined -> [];
	       {ok, O1} -> O1
	   end,
    hex_sup:start_link(Opts).

stop(_State) ->
    ok.
