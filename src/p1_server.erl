%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% The code has been modified and improved by ProcessOne.
%%
%% Copyright 2007-2025   ProcessOne
%%
%%  The change adds the following features:
%%   - You can send exit(priority_shutdown) to the p1_fsm process to
%%   terminate immediatetly. If the fsm trap_exit process flag has been
%%   set to true, the FSM terminate function will called.
%%   - You can pass the gen_fsm options to control resource usage.
%%   {max_queue, N} will exit the process with priority_shutdown
%%   - You can limit the time processing a message (TODO): If the
%%   message processing does not return in a given period of time, the
%%   process will be terminated.
%%   - You might customize the State data before sending it to error_logger
%%   in case of a crash (just export the function print_state/1)
%%
-module(p1_server).

%%% ---------------------------------------------------
%%%
%%% The idea behind THIS server is that the user module
%%% provides (different) functions to handle different
%%% kind of inputs.
%%% If the Parent process terminates the Module:terminate/2
%%% function is called.
%%%
%%% The user module should export:
%%%
%%%   init(Args)
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, {From, Tag}, State)
%%%
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State)
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State) Info is e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the server) can be described as follows:
%%%
%%%   User module                          Generic
%%%   -----------                          -------
%%%     start            ----->             start
%%%     init             <-----              .
%%%
%%%                                         loop
%%%     handle_call      <-----              .
%%%                      ----->             reply
%%%
%%%     handle_cast      <-----              .
%%%
%%%     handle_info      <-----              .
%%%
%%%     terminate        <-----              .
%%%
%%%                      ----->             reply
%%%
%%%
%%% ---------------------------------------------------

%% API
-export([start/3, start/4,
	 start_link/3, start_link/4,
	 call/2, call/3,
	 cast/2, reply/2,
	 abcast/2, abcast/3,
	 multi_call/2, multi_call/3, multi_call/4,
	 enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/6]).

%% System exports
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
         system_get_state/1,
         system_replace_state/2,
	 format_status/2]).

%% Internal exports
-export([init_it/6]).

-import(error_logger, [format/2]).

%%% Internal gen_fsm state
%%% This state is used to defined resource control values:
-record(limits, {max_queue :: non_neg_integer() | undefined}).

%%%=========================================================================
%%%  API
%%%=========================================================================

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

%%%  -----------------------------------------------------------------
%%% Starts a generic server.
%%% start(Mod, Args, Options)
%%% start(Name, Mod, Args, Options)
%%% start_link(Mod, Args, Options)
%%% start_link(Name, Mod, Args, Options) where:
%%%    Name ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%%    Mod  ::= atom(), callback module implementing the 'real' server
%%%    Args ::= term(), init arguments (to Mod:init/1)
%%%    Options ::= [{timeout, Timeout} | {debug, [Flag]}]
%%%      Flag ::= trace | log | {logfile, File} | statistics | debug
%%%          (debug == log && statistics)
%%% Returns: {ok, Pid} |
%%%          {error, {already_started, Pid}} |
%%%          {error, Reason}
%%% -----------------------------------------------------------------
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% -----------------------------------------------------------------
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({via, Mod, Name}, Request) ->
    catch Mod:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) ->
    do_send(Dest, cast_msg(Request)),
    ok.

cast_msg(Request) -> {'$gen_cast',Request}.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
reply({To, Tag}, Reply) ->
    gen:reply({To, Tag}, Reply).

%% -----------------------------------------------------------------
%% Asynchronous broadcast, returns nothing, it's just send 'n' pray
%%-----------------------------------------------------------------
abcast(Name, Request) when is_atom(Name) ->
    do_abcast([node() | nodes()], Name, cast_msg(Request)).

abcast(Nodes, Name, Request) when is_list(Nodes), is_atom(Name) ->
    do_abcast(Nodes, Name, cast_msg(Request)).

do_abcast([Node|Nodes], Name, Msg) when is_atom(Node) ->
    do_send({Name,Node},Msg),
    do_abcast(Nodes, Name, Msg);
do_abcast([], _,_) -> abcast.

%%% -----------------------------------------------------------------
%%% Make a call to servers at several nodes.
%%% Returns: {[Replies],[BadNodes]}
%%% A Timeout can be given
%%%
%%% A middleman process is used in case late answers arrives after
%%% the timeout. If they would be allowed to glog the callers message
%%% queue, it would probably become confused. Late answers will
%%% now arrive to the terminated middleman and so be discarded.
%%% -----------------------------------------------------------------
multi_call(Name, Req)
  when is_atom(Name) ->
    do_multi_call([node() | nodes()], Name, Req, infinity).

multi_call(Nodes, Name, Req)
  when is_list(Nodes), is_atom(Name) ->
    do_multi_call(Nodes, Name, Req, infinity).

multi_call(Nodes, Name, Req, infinity) ->
    do_multi_call(Nodes, Name, Req, infinity);
multi_call(Nodes, Name, Req, Timeout)
  when is_list(Nodes), is_atom(Name), is_integer(Timeout), Timeout >= 0 ->
    do_multi_call(Nodes, Name, Req, Timeout).


%%-----------------------------------------------------------------
%% enter_loop(Mod, Options, State, <ServerName>, <TimeOut>) ->_
%%
%% Description: Makes an existing process into a gen_server.
%%              The calling process will enter the gen_server receive
%%              loop and become a gen_server process.
%%              The process *must* have been started using one of the
%%              start functions in proc_lib, see proc_lib(3).
%%              The user is responsible for any initialization of the
%%              process, including registering a name for it.
%%-----------------------------------------------------------------
enter_loop(Mod, Options, State) ->
    enter_loop(Mod, Options, State, self(), infinity).

enter_loop(Mod, Options, State, ServerName = {Scope, _})
  when Scope == local; Scope == global ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, ServerName = {via, _, _}) ->
    enter_loop(Mod, Options, State, ServerName, infinity);

enter_loop(Mod, Options, State, Timeout) ->
    enter_loop(Mod, Options, State, self(), Timeout).

enter_loop(Mod, Options, State, ServerName, Timeout) ->
    Name = get_proc_name(ServerName),
    Parent = get_parent(),
    Debug = debug_options(Name, Options),
    Limits = limit_options(Options),
    Queue = queue:new(),
    QueueLen = 0,
    loop(Parent, Name, State, Mod, Timeout, Debug,
         Limits, Queue, QueueLen).

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, Mod, Args, Options) ->
    Name = name(Name0),
    Debug = debug_options(Name, Options),
    Limits = limit_options(Options),
    Queue = queue:new(),
    QueueLen = 0,
    case catch Mod:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(Parent, Name, State, Mod, infinity, Debug,
                 Limits, Queue, QueueLen);
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    loop(Parent, Name, State, Mod, Timeout, Debug,
                 Limits, Queue, QueueLen);
	{stop, Reason} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.)
	    unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    unregister_name(Name0),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    unregister_name(Name0),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    _ = (catch unregister(Name));
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name);
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
loop(Parent, Name, State, Mod, hibernate, Debug,
     Limits, Queue, QueueLen)
  when QueueLen > 0 ->
    case queue:out(Queue) of
	{{value, Msg}, Queue1} ->
	    decode_msg(Msg, Parent, Name, State, Mod, hibernate,
		       Debug, Limits, Queue1, QueueLen - 1, false);
	{empty, _} ->
	    Reason = internal_queue_error,
	    error_info(Mod, Reason, Name, hibernate, State, Debug),
	    exit(Reason)
    end;
loop(Parent, Name, State, Mod, hibernate, Debug,
     Limits, _Queue, _QueueLen) ->
    proc_lib:hibernate(?MODULE,wake_hib,[Parent, Name, State, Mod, Debug,
                                         Limits]);
%% First we test if we have reach a defined limit ...
loop(Parent, Name, State, Mod, Time, Debug,
     Limits, Queue, QueueLen) ->
    try
	message_queue_len(Limits, QueueLen)
	%% TODO: We can add more limit checking here...
    catch
	{process_limit, Limit} ->
	    Reason = {process_limit, Limit},
	    Msg = {'EXIT', Parent, {error, {process_limit, Limit}}},
	    terminate(Reason, Name, Msg, Mod, State, Debug,
                      queue:new())
    end,
    process_message(Parent, Name, State, Mod, Time, Debug,
                    Limits, Queue, QueueLen).

%% ... then we can process a new message:
process_message(Parent, Name, State, Mod, Time, Debug,
		Limits, Queue, QueueLen) ->
    {Msg, Queue1, QueueLen1} = collect_messages(Queue, QueueLen, Time),
    decode_msg(Msg, Parent, Name, State, Mod, Time, Debug,
               Limits, Queue1, QueueLen1, false).

collect_messages(Queue, QueueLen, Time) ->
    receive
	Input ->
	    case Input of
		{'EXIT', _Parent, priority_shutdown} ->
		    {Input, Queue, QueueLen};
		_ ->
		    collect_messages(
		      queue:in(Input, Queue), QueueLen + 1, Time)
	    end
    after 0 ->
	    case queue:out(Queue) of
		{{value, Msg}, Queue1} ->
		    {Msg, Queue1, QueueLen - 1};
		{empty, _} ->
		    receive
			Input ->
			    {Input, Queue, QueueLen}
		    after Time ->
			    {timeout, Queue, QueueLen}
		    end
	    end
    end.

wake_hib(Parent, Name, State, Mod, Debug,
	 Limits) ->
    Msg = receive
	      Input ->
		  Input
	  end,
    Queue = queue:new(),
    QueueLen = 0,
    decode_msg(Msg, Parent, Name, State, Mod, hibernate, Debug,
               Limits, Queue, QueueLen, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, Debug,
	   Limits, Queue, QueueLen, Hib) ->
    put('$internal_queue_len', QueueLen),
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time,
                                   Limits, Queue, QueueLen], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Debug, Queue);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod,
                       Limits, Queue, QueueLen);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, Debug1,
                       Limits, Queue, QueueLen)
    end.

%%% ---------------------------------------------------
%%% Send/receive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    case catch erlang:send(Dest, Msg, [noconnect]) of
	noconnect ->
	    spawn(erlang, send, [Dest,Msg]);
	Other ->
	    Other
    end.

do_multi_call(Nodes, Name, Req, infinity) ->
    Tag = make_ref(),
    Monitors = send_nodes(Nodes, Name, Tag, Req),
    rec_nodes(Tag, Monitors, Name, undefined);
do_multi_call(Nodes, Name, Req, Timeout) ->
    Tag = make_ref(),
    Caller = self(),
    Receiver =
	spawn(
	  fun() ->
		  %% Middleman process. Should be unsensitive to regular
		  %% exit signals. The synchronization is needed in case
		  %% the receiver would exit before the caller started
		  %% the monitor.
		  process_flag(trap_exit, true),
		  Mref = erlang:monitor(process, Caller),
		  receive
		      {Caller,Tag} ->
			  Monitors = send_nodes(Nodes, Name, Tag, Req),
			  TimerId = erlang:start_timer(Timeout, self(), ok),
			  Result = rec_nodes(Tag, Monitors, Name, TimerId),
			  exit({self(),Tag,Result});
		      {'DOWN',Mref,_,_,_} ->
			  %% Caller died before sending us the go-ahead.
			  %% Give up silently.
			  exit(normal)
		  end
	  end),
    Mref = erlang:monitor(process, Receiver),
    Receiver ! {self(),Tag},
    receive
	{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
	    Result;
	{'DOWN',Mref,_,_,Reason} ->
	    %% The middleman code failed. Or someone did
	    %% exit(_, kill) on the middleman process => Reason==killed
	    exit(Reason)
    end.

send_nodes(Nodes, Name, Tag, Req) ->
    send_nodes(Nodes, Name, Tag, Req, []).

send_nodes([Node|Tail], Name, Tag, Req, Monitors)
  when is_atom(Node) ->
    Monitor = start_monitor(Node, Name),
    %% Handle non-existing names in rec_nodes.
    catch {Name, Node} ! {'$gen_call', {self(), {Tag, Node}}, Req},
    send_nodes(Tail, Name, Tag, Req, [Monitor | Monitors]);
send_nodes([_Node|Tail], Name, Tag, Req, Monitors) ->
    %% Skip non-atom Node
    send_nodes(Tail, Name, Tag, Req, Monitors);
send_nodes([], _Name, _Tag, _Req, Monitors) ->
    Monitors.

%% Against old nodes:
%% If no reply has been delivered within 2 secs. (per node) check that
%% the server really exists and wait for ever for the answer.
%%
%% Against contemporary nodes:
%% Wait for reply, server 'DOWN', or timeout from TimerId.

rec_nodes(Tag, Nodes, Name, TimerId) ->
    rec_nodes(Tag, Nodes, Name, [], [], 2000, TimerId).

rec_nodes(Tag, [{N,R}|Tail], Name, Badnodes, Replies, Time, TimerId ) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, Time, TimerId);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    erlang:demonitor(R, [flush]),
	    rec_nodes(Tag, Tail, Name, Badnodes,
		      [{N,Reply}|Replies], Time, TimerId);
	{timeout, TimerId, _} ->
	    erlang:demonitor(R, [flush]),
	    %% Collect all replies that already have arrived
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes(Tag, [N|Tail], Name, Badnodes, Replies, Time, TimerId) ->
    %% R6 node
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes(Tag, Tail, Name, [N|Badnodes], Replies, 2000, TimerId);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes(Tag, Tail, Name, Badnodes,
		      [{N,Reply}|Replies], 2000, TimerId);
	{timeout, TimerId, _} ->
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    %% Collect all replies that already have arrived
	    rec_nodes_rest(Tag, Tail, Name, [N | Badnodes], Replies)
    after Time ->
	    case rpc:call(N, erlang, whereis, [Name]) of
		Pid when is_pid(Pid) -> % It exists try again.
		    rec_nodes(Tag, [N|Tail], Name, Badnodes,
			      Replies, infinity, TimerId);
		_ -> % badnode
		    receive {nodedown, N} -> ok after 0 -> ok end,
		    monitor_node(N, false),
		    rec_nodes(Tag, Tail, Name, [N|Badnodes],
			      Replies, 2000, TimerId)
	    end
    end;
rec_nodes(_, [], _, Badnodes, Replies, _, TimerId) ->
    case catch erlang:cancel_timer(TimerId) of
	false ->  % It has already sent it's message
	    receive
		{timeout, TimerId, _} -> ok
	    after 0 ->
		    ok
	    end;
	_ -> % Timer was cancelled, or TimerId was 'undefined'
	    ok
    end,
    {Replies, Badnodes}.

%% Collect all replies that already have arrived
rec_nodes_rest(Tag, [{N,R}|Tail], Name, Badnodes, Replies) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
	{{Tag, N}, Reply} -> %% Tag is bound !!!
	    erlang:demonitor(R, [flush]),
	    rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
    after 0 ->
	    erlang:demonitor(R, [flush]),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes_rest(Tag, [N|Tail], Name, Badnodes, Replies) ->
    %% R6 node
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies);
	{{Tag, N}, Reply} ->  %% Tag is bound !!!
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, Badnodes, [{N,Reply}|Replies])
    after 0 ->
	    receive {nodedown, N} -> ok after 0 -> ok end,
	    monitor_node(N, false),
	    rec_nodes_rest(Tag, Tail, Name, [N|Badnodes], Replies)
    end;
rec_nodes_rest(_Tag, [], _Name, Badnodes, Replies) ->
    {Replies, Badnodes}.


%%% ---------------------------------------------------
%%% Monitor functions
%%% ---------------------------------------------------

start_monitor(Node, Name) when is_atom(Node), is_atom(Name) ->
    if node() =:= nonode@nohost, Node =/= nonode@nohost ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
	    {Node, Ref};
       true ->
	    case catch erlang:monitor(process, {Name, Node}) of
		{'EXIT', _} ->
		    %% Remote node is R6
		    monitor_node(Node, true),
		    Node;
		Ref when is_reference(Ref) ->
		    {Node, Ref}
	    end
    end.

%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------

dispatch({'$gen_cast', Msg}, Mod, State) ->
    Mod:handle_cast(Msg, State);
dispatch(Info, Mod, State) ->
    Mod:handle_info(Info, State).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod,
	   Limits, Queue, QueueLen) ->
    case catch Mod:handle_call(Msg, From, State) of
	{reply, Reply, NState} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, infinity, [],
                 Limits, Queue, QueueLen);
	{reply, Reply, NState, Time1} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, Time1, [],
                 Limits, Queue, QueueLen);
	{noreply, NState} ->
	    loop(Parent, Name, NState, Mod, infinity, [],
                 Limits, Queue, QueueLen);
	{noreply, NState, Time1} ->
	    loop(Parent, Name, NState, Mod, Time1, [],
                 Limits, Queue, QueueLen);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} =
		(catch terminate(Reason, Name, Msg, Mod, NState, [], Queue)),
	    reply(From, Reply),
	    exit(R);
	Other -> handle_common_reply(Other, Parent, Name, Msg, Mod, State,
                                     Limits, Queue, QueueLen)
    end;
handle_msg(Msg, Parent, Name, State, Mod,
	   Limits, Queue, QueueLen) ->
    Reply = (catch dispatch(Msg, Mod, State)),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State,
                        Limits, Queue, QueueLen).

handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug,
	   Limits, Queue, QueueLen) ->
    case catch Mod:handle_call(Msg, From, State) of
	{reply, Reply, NState} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, infinity, Debug1,
                 Limits, Queue, QueueLen);
	{reply, Reply, NState, Time1} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, Time1, Debug1,
                 Limits, Queue, QueueLen);
	{noreply, NState} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1,
                 Limits, Queue, QueueLen);
	{noreply, NState, Time1} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1,
                 Limits, Queue, QueueLen);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} =
		(catch terminate(Reason, Name, Msg, Mod, NState, Debug, Queue)),
	    reply(Name, From, Reply, NState, Debug),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug,
                                Limits, Queue, QueueLen)
    end;
handle_msg(Msg, Parent, Name, State, Mod, Debug,
	   Limits, Queue, QueueLen) ->
    Reply = (catch dispatch(Msg, Mod, State)),
    handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug,
                        Limits, Queue, QueueLen).

handle_common_reply(Reply, Parent, Name, Msg, Mod, State,
                    Limits, Queue, QueueLen) ->
    case Reply of
	{noreply, NState} ->
	    loop(Parent, Name, NState, Mod, infinity, [],
                 Limits, Queue, QueueLen);
	{noreply, NState, Time1} ->
	    loop(Parent, Name, NState, Mod, Time1, [],
                 Limits, Queue, QueueLen);
	{stop, Reason, NState} ->
	    terminate(Reason, Name, Msg, Mod, NState, [], Queue);
	{'EXIT', What} ->
	    terminate(What, Name, Msg, Mod, State, [], Queue);
	_ ->
	    terminate({bad_return_value, Reply}, Name, Msg, Mod, State, [],
                      Queue)
    end.

handle_common_reply(Reply, Parent, Name, Msg, Mod, State, Debug,
                    Limits, Queue, QueueLen) ->
    case Reply of
	{noreply, NState} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1,
                 Limits, Queue, QueueLen);
	{noreply, NState, Time1} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3, Name,
				      {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1,
                 Limits, Queue, QueueLen);
	{stop, Reason, NState} ->
	    terminate(Reason, Name, Msg, Mod, NState, Debug, Queue);
	{'EXIT', What} ->
	    terminate(What, Name, Msg, Mod, State, Debug, Queue);
	_ ->
	    terminate({bad_return_value, Reply}, Name, Msg, Mod, State, Debug,
                      Queue)
    end.

reply(Name, {To, Tag}, Reply, State, Debug) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, fun print_event/3, Name,
		     {out, Reply, To, State} ).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [Name, State, Mod, Time,
                                Limits, Queue, QueueLen]) ->
    loop(Parent, Name, State, Mod, Time, Debug,
         Limits, Queue, QueueLen).

-spec system_terminate(_, _, _, [_]) -> no_return().

system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time,
                                          _Limits, Queue, _QueueLen]) ->
    terminate(Reason, Name, [], Mod, State, Debug, Queue).

system_code_change([Name, State, Mod, Time,
		    Limits, Queue, QueueLen], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
	{ok, NewState} -> {ok, [Name, NewState, Mod, Time,
                                Limits, Queue, QueueLen]};
	Else -> Else
    end.

system_get_state([_Name, State, _Mod, _Time,
                  _Limits, _Queue, _QueueLen]) ->
    {ok, State}.

system_replace_state(StateFun,
                     [Name, State, Mod, Time,
                      Limits, Queue, QueueLen]) ->
    NState = StateFun(State),
    {ok, NState, [Name, NState, Mod, Time,
                  Limits, Queue, QueueLen]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got cast ~p~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n",
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).


%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, Name, Msg, Mod, State, Debug, Queue) ->
    lists:foreach(
      fun(Message) -> self() ! Message end,
      queue:to_list(Queue)),
    case catch Mod:terminate(Reason, State) of
	{'EXIT', R} ->
	    error_info(Mod, R, Name, Msg, State, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		{shutdown,_}=Shutdown ->
		    exit(Shutdown);
		priority_shutdown ->
		    %% Priority shutdown should be considered as
		    %% shutdown by SASL
		    exit(shutdown);
		{process_limit, _Limit} ->
		    exit(Reason);
		_ ->
		    FmtState =
			case erlang:function_exported(Mod, format_status, 2) of
			    true ->
				Args = [get(), State],
				case catch Mod:format_status(terminate, Args) of
				    {'EXIT', _} -> State;
				    Else -> Else
				end;
			    _ ->
				State
			end,
		    error_info(Mod, Reason, Name, Msg, FmtState, Debug),
		    exit(Reason)
	    end
    end.

error_info(_Mod, _Reason, application_controller, _Msg, _State, _Debug) ->
    %% OTP-5811 Don't send an error report if it's the system process
    %% application_controller which is terminating - let init take care
    %% of it instead
    ok;
error_info(Mod, Reason, Name, Msg, State, Debug) ->
    Reason1 =
	case Reason of
	    {undef,[{M,F,A,L}|MFAs]} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A,L}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				Reason;
			    false ->
				{'function not exported',[{M,F,A,L}|MFAs]}
			end
		end;
	    _ ->
		Reason
	end,
    StateToPrint = case erlang:function_exported(Mod, print_state, 1) of
      true -> (catch Mod:print_state(State));
      false -> State
    end,
    format("** Generic server ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
	   [Name, Msg, StateToPrint, Reason1]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------

opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Name, Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> dbg_opts(Name, Options);
	_ -> dbg_opts(Name, [])
    end.

dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
	{'EXIT',_} ->
	    format("~p: ignoring erroneous debug options - ~p~n",
		   [Name, Opts]),
	    [];
	Dbg ->
	    Dbg
    end.

get_proc_name(Pid) when is_pid(Pid) ->
    Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
	{registered_name, Name} ->
	    Name;
	{registered_name, _Name} ->
	    exit(process_not_registered);
	[] ->
	    exit(process_not_registered)
    end;
get_proc_name({global, Name}) ->
    case global:whereis_name(Name) of
	undefined ->
	    exit(process_not_registered_globally);
	Pid when Pid =:= self() ->
	    Name;
	_Pid ->
	    exit(process_not_registered_globally)
    end;
get_proc_name({via, Mod, Name}) ->
    case Mod:whereis_name(Name) of
	undefined ->
	    exit({process_not_registered_via, Mod});
	Pid when Pid =:= self() ->
	    Name;
	_Pid ->
	    exit({process_not_registered_via, Mod})
    end.

get_parent() ->
    case get('$ancestors') of
	[Parent | _] when is_pid(Parent)->
            Parent;
        [Parent | _] when is_atom(Parent)->
            name_to_pid(Parent);
	_ ->
	    exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
	undefined ->
	    case global:whereis_name(Name) of
		undefined ->
		    exit(could_not_find_registered_name);
		Pid ->
		    Pid
	    end;
	Pid ->
	    Pid
    end.

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time,
                                      _Limits, _Queue, _QueueLen]] = StatusData,
    Header = gen:format_status_header("Status for generic server",
                                      Name),
    Log = sys_get_debug(log, Debug, []),
    DefaultStatus = [{data, [{"State", State}]}],
    Specific =
	case erlang:function_exported(Mod, format_status, 2) of
	    true ->
		case catch Mod:format_status(Opt, [PDict, State]) of
		    {'EXIT', _} -> DefaultStatus;
                    StatusList when is_list(StatusList) -> StatusList;
		    Else -> [Else]
		end;
	    _ ->
		DefaultStatus
	end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log}]} |
     Specific].

-ifdef(USE_OLD_SYS_GET_DEBUG).
sys_get_debug(Item, Debug, Default) -> sys:get_debug(Item, Debug, Default).
-else.
sys_get_debug(log, Debug, _Default) -> sys:get_log(Debug).
-endif.

%%-----------------------------------------------------------------
%% Resources limit management
%%-----------------------------------------------------------------
%% Extract know limit options
limit_options(Options) ->
    limit_options(Options, #limits{}).
limit_options([], Limits) ->
    Limits;
%% Maximum number of messages allowed in the process message queue
limit_options([{max_queue,N}|Options], Limits)
  when is_integer(N) ->
    NewLimits = Limits#limits{max_queue=N},
    limit_options(Options, NewLimits);
limit_options([_|Options], Limits) ->
    limit_options(Options, Limits).

%% Throw max_queue if we have reach the max queue size
%% Returns ok otherwise
message_queue_len(#limits{max_queue = undefined}, _QueueLen) ->
    ok;
message_queue_len(#limits{max_queue = MaxQueue}, QueueLen) ->
    Pid = self(),
    case process_info(Pid, message_queue_len) of
        {message_queue_len, N} when N + QueueLen > MaxQueue ->
	    throw({process_limit, {max_queue, N + QueueLen}});
	_ ->
	    ok
    end.
