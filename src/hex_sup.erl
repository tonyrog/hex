-module(hex_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    Server = {hex_server, {hex_server, start_link, [Args]},
	      permanent, 5000, worker, [hex_server]},
    {ok, { {one_for_one,3,5}, [Server]} }.
