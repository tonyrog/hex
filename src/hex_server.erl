%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex main processing server
%%% @end
%%% Created :  3 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([reload/0, load/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([output/2, input/2]).
-export([match_value/2, match_pattern/2]).


-include("../include/hex.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, hex_table).

-record(state, {
	  file = "" :: string(),
	  nodeid = 0 :: integer(),
	  tab :: ets:tab(),
	  out_list = []    :: [{Label::integer(), Pid::pid()}],
	  in_list  = []    :: [{Label::integer(), Pid::pid()}],
	  evt_list = []    :: [{Label::integer(), Ref::reference()}],
	  input_rules = [] :: [#hex_input{}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================

reload() ->
    gen_server:call(?SERVER, reload).

load(File) ->
    gen_server:call(?SERVER, {load,File}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    Tab = ets:new(?TABLE, [named_table]),
    Nodeid = proplists:get_value(nodeid, Options, 1),
    Name = proplists:get_value(config, Options, "local.conf"),
    File = case filename:dirname(Name) of
	       "." -> filename:join(code:priv_dir(hex), Name);
	       _ -> Name
	   end,
    lager:debug("starting hex_server nodeid=~.16B, config=~s",
		[Nodeid, File]),
    self() ! reload,
    {ok, #state{ nodeid = Nodeid,
		 file = File,
		 tab = Tab,
		 input_rules = []
	       }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({load,File}, _From, State) ->
    case reload(File, State) of
	{ok,State1} ->
	    {reply, ok, State1#state { file = File}};
	Error ->
	    {reply, Error, State}
    end;
handle_call(reload, _From, State) ->
    case reload(State#state.file, State) of
	{ok,State1} ->
	    {reply, ok, State1};
	Error ->
	    {reply, Error, State}
    end;    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Sig=#hex_signal{}, State) ->
    lager:debug("signal input: ~p\n", [Sig]),
    run(Sig, State#state.input_rules),
    {noreply, State};

handle_info(reload, State) ->
    case reload(State#state.file, State) of
	{ok,State1} ->
	    {noreply, State1};
	_Error ->
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reload(File, State) ->
    case file:consult(File) of
	{ok,Config} ->
	    case hex_config:scan(Config) of
		{ok,{Evt,In,Out}} ->
		    State1 = start_outputs(Out, State),
		    State2 = start_inputs(In, State1),
		    State3 = start_events(Evt, State2),
		    {ok, State3#state { input_rules = In }};
		Error={error,Reason} ->
		    io:format("error loading ~s\n~p\n", [File,Reason]),
		    Error
	    end;
	Error={error,Reason} ->
	    io:format("error loading ~s\n~p\n", [File,Reason]),
	    Error
    end.


start_outputs([#hex_output { label=L, flags=Flags, actions=Actions} | Out],
	      State) ->
    case Actions of
	{App,AppFlags} -> 
	    start_plugin(App,out,AppFlags);
	_ when is_list(Actions) ->
	    lists:foreach(
	      fun({_Pattern,{App,AppFlags}}) ->
		      start_plugin(App,out,AppFlags) 
	      end, Actions)
    end,
    %% nodeid and chan is needed for feedback from output
    Flags1 = [{nodeid,State#state.nodeid},{chan,L}|Flags],
    {ok,Pid} = hex_output:start_link(Flags1, Actions),
    ets:insert(State#state.tab, {{output,L}, Pid}),
    OutList = [{L,Pid} | State#state.out_list],
    start_outputs(Out, State#state { out_list = OutList });
start_outputs([], State) ->
    State.

start_inputs([#hex_input { label=L, flags = Flags, output=Output } | In],
	     State) ->
    {ok,Pid} = hex_input:start_link([{output,Output}|Flags]),
    ets:insert(State#state.tab, {{input,L}, Pid}),
    InList = [{L,Pid} | State#state.in_list],
    start_inputs(In, State#state { in_list = InList });
start_inputs([], State) ->
    State.

%% start 
start_events([#hex_event { label=L,app=App,flags=Flags,signal=Signal } | Evt],
	     State) ->
    case start_plugin(App, in, Flags) of
	ok ->
	    io:format("add_event: ~p ~p\n", [App,Flags]),  
	    {ok,Ref} = App:add_event(Flags, Signal),
	    io:format("event ~w started ~w\n", [L, Ref]),
	    EvtList = [{L,Ref} | State#state.evt_list],
	    start_events(Evt, State#state { evt_list = EvtList });
	_Error ->
	    start_events(Evt, State)
    end;
start_events([], State) ->
    State.


start_plugin(App, Dir, Flags) ->
    case application:ensure_all_started(App) of
	{ok,[]} -> %% already started
	    init_plugin(App, Dir, Flags);	    
	{ok,Started} ->
	    lager:info("plugin ~w started: ~p", [App,Started]),
	    init_plugin(App, Dir, Flags);
	Error ->
	    lager:error("plugin ~w failed to start: ~p", [App, Error]),
	    Error
    end.

init_plugin(App, Dir, Flags) ->
    case App:init_event(Dir, Flags) of
	ok ->
	    lager:info("~w:~w event ~p initiated", [App,Dir,Flags]),
	    ok;
	Error ->
	    lager:info("~w:~w event ~p failed ~p", [App,Dir,Flags,Error]),
	    Error
    end.


run(Signal, Rules) when is_record(Signal, hex_signal) ->
    run_(Signal, Rules).

run_(Sig, [Rule|Rules]) ->
    lager:debug("run ~p rule=~p", [Sig, Rule]),
    case match_pattern(Sig, Rule#hex_input.signal) of
	{true,Value} ->
	    Src = Sig#hex_signal.source,
	    V = case Sig#hex_signal.type of
		    ?HEX_DIGITAL -> {digital,Value,Src};
		    ?HEX_ANALOG ->  {analog,Value,Src};
		    ?HEX_ENCODER -> {encoder,Value,Src};
		    ?HEX_RFID    -> {rfid,Value,Src};
		    Type -> {Type,Value,Src}
		end,
	    input(Rule#hex_input.label, V),
	    run_(Sig, Rules);
	false ->
	    run_(Sig, Rules)
    end;
run_(_Sig, []) ->
    ok.

match_pattern(Sig, Pat) when is_record(Sig, hex_signal),
			     is_record(Pat, hex_pattern) ->
    case match_value(Pat#hex_pattern.id,Sig#hex_signal.id) andalso 
	match_value(Pat#hex_pattern.chan,Sig#hex_signal.chan) andalso 
	match_value(Pat#hex_pattern.type, Sig#hex_signal.type) of
	true ->
	    case match_value(Pat#hex_pattern.value,Sig#hex_signal.value) of
		true -> {true, Sig#hex_signal.value};
		false -> false
	    end;
	false ->
	    false
    end.

match_value(A, A) -> true;
match_value({mask,Mask,Match}, A) -> A band Mask =:= Match;
match_value({range,Low,High}, A) -> (A >= Low) andalso (A =< High);
match_value({'not',Cond}, A) -> not match_value(Cond,A);
match_value({'and',C1,C2}, A) -> match_value(C1,A) andalso match_value(C2,A);
match_value({'or',C1,C2}, A) -> match_value(C1,A) orelse match_value(C2,A);
match_value([Cond|Cs], A) -> match_value(Cond,A) andalso match_value(Cs, A);
match_value([], _A) -> true;
match_value(_, _) -> false.


input(I, Value) when is_integer(I); is_atom(I) ->
    lager:debug("input ~w ~w", [I, Value]),
    try ets:lookup(?TABLE, {input,I}) of
	[] ->
	    io:format("warning: hex_input ~w not running\n", [I]),
	    ignore;
	[{_,Fsm}] ->
	    gen_fsm:send_event(Fsm, Value)
    catch 
	error:badarg ->
	    io:format("warning: hex_server not running\n", []),
	    ignore
    end.

%% called from hex_input !
output(O, Value) when is_integer(O) ->
    lager:debug("output ~w ~w", [O, Value]),
    try ets:lookup(?TABLE, {output,O}) of
	[] -> 
	    lager:warnin("output ~w not running", [O]),
	    ignore;
	[{_,Fsm}] ->
	    gen_fsm:send_event(Fsm, Value)
    catch
	error:badarg ->
	    lager:warning("hex_server not running", []),
	    ignore
    end.
