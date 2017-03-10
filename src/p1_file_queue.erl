%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(p1_file_queue).

-behaviour(p1_server).

%% API
-export([new/0, is_queue/1, len/1, is_empty/1, in/2, out/1,
	 peek/1, drop/1, from_list/1, to_list/1, foreach/2,
	 foldl/3, dropwhile/2, path/1, clear/1, format_error/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([start/1, stop/0, start_link/1]).
%% For tests only
-export([close/1]).

-include("p1_queue.hrl").

-record(state, {dir :: filename:filename(),
		counter :: pos_integer(),
		files :: map()}).

-type queue() :: #file_q{}.
-export_type([queue/0]).

-define(MAX_QUEUES_PER_PROCESS, 10).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    case get_filename() of
	{ok, Path} ->
	    case file:open(Path, [read, write, binary, raw]) of
		{ok, Fd} ->
		    monitor_me(Path),
		    clear(#file_q{fd = Fd, path = Path});
		{error, Err} ->
		    erlang:error(Err)
	    end;
	{error, Err} ->
	    erlang:error(Err)
    end.

path(#file_q{path = Path}) ->
    Path.

is_queue(#file_q{}) -> true;
is_queue(_) -> false.

len(#file_q{tail = Tail}) ->
    Tail.

is_empty(#file_q{tail = Tail}) ->
    Tail == 0.

in(Item, #file_q{start = Pos, stop = Pos} = Q) when Pos /= 0 ->
    in(Item, clear(Q));
in(Item, #file_q{fd = Fd, tail = Tail, stop = Pos} = Q) ->
    Data = term_to_binary(Item),
    Size = size(Data),
    case file:pwrite(Fd, Pos, <<Size:32, Data/binary>>) of
	ok ->
	    gc(Q#file_q{tail = Tail + 1, stop = Pos + Size + 4});
	{error, Err} ->
	    erlang:error(Err)
    end.

out(#file_q{tail = 0} = Q) ->
    {empty, Q};
out(#file_q{fd = Fd, tail = Tail, head = Head, start = Pos} = Q) ->
    case read_item(Fd, Pos) of
	{ok, Item, Next} ->
	    {{value, Item},
	     Q#file_q{tail = Tail - 1, head = Head + 1, start = Next}};
	{error, Err} ->
	    erlang:error(Err)
    end.

peek(#file_q{tail = 0}) ->
    empty;
peek(#file_q{fd = Fd, start = Pos}) ->
    case read_item(Fd, Pos) of
	{ok, Item, _} ->
	    {value, Item};
	{error, Err} ->
	    erlang:error(Err)
    end.

drop(#file_q{tail = 0}) ->
    erlang:error(empty);
drop(#file_q{fd = Fd, start = Pos, tail = Tail, head = Head} = Q) ->
    case read_item_size(Fd, Pos) of
	{ok, Size} ->
	    Q#file_q{tail = Tail - 1, head = Head + 1, start = Pos + Size + 4};
	{error, Err} ->
	    erlang:error(Err)
    end.

from_list(Items) ->
    Q = #file_q{fd = Fd} = new(),
    {Tail, Stop} = lists:foldl(
		    fun(Item, {Len, Pos}) ->
			    Data = term_to_binary(Item),
			    Size = size(Data),
			    case file:write(Fd, <<Size:32, Data/binary>>) of
				ok ->
				    {Len + 1, Pos + Size + 4};
				{error, Err} ->
				    erlang:error(Err)
			    end
		    end, {0, 0}, Items),
    Q#file_q{tail = Tail, stop = Stop}.

to_list(#file_q{fd = Fd, tail = Tail, start = Pos}) ->
    to_list(Fd, Pos, Tail, []).

dropwhile(F, Q) ->
    case peek(Q) of
	{value, Item} ->
	    case F(Item) of
		true ->
		    dropwhile(F, drop(Q));
		_ ->
		    Q
	    end;
	empty ->
	    Q
    end.

foldl(F, Acc, Q) ->
    case out(Q) of
	{{value, Item}, Q1} ->
	    Acc1 = F(Item, Acc),
	    foldl(F, Acc1, Q1);
	{empty, _} ->
	    Acc
    end.

foreach(F, Q) ->
    case out(Q) of
	{{value, Item}, Q1} ->
	    F(Item),
	    foreach(F, Q1);
	{empty, _} ->
	    ok
    end.

clear(#file_q{fd = Fd, path = Path}) ->
    case file:position(Fd, 0) of
	{ok, 0} ->
	    case file:truncate(Fd) of
		ok ->
		    #file_q{fd = Fd, path = Path};
		{error, Err} ->
		    erlang:error(Err)
	    end;
	{error, Err} ->
	    erlang:error(Err)
    end.

close(#file_q{fd = Fd, path = Path}) ->
    file:close(Fd),
    demonitor_me(Path).

format_error(empty) ->
    "file queue empty";
format_error(corrupted) ->
    "file queue corrupted";
format_error(Posix) ->
    file:format_error(Posix).

%%%===================================================================
%%% p1_server API
%%%===================================================================
start(Dir) ->
    Spec = {?MODULE, {?MODULE, start_link, [Dir]},
	    permanent, 5000, worker, [?MODULE]},
    supervisor:start_child(p1_utils_sup, Spec).

stop() ->
    supervisor:terminate_child(p1_utils_sup, ?MODULE),
    supervisor:delete_child(p1_utils_sup, ?MODULE).

start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dir], []).

init([Dir]) ->
    case filelib:ensure_dir(filename:join(Dir, "foo")) of
	ok ->
	    crypto:start(),
	    process_flag(trap_exit, true),
	    {ok, #state{dir = Dir, files = #{}, counter = 0}};
	{error, Reason} ->
	    error_logger:error_msg(
	      "failed to create directory \"~s\": ~s",
	      [Dir, file:format_error(Reason)]),
	    {stop, Reason}
    end.

handle_call({get_filename, Owner}, _, #state{dir = Dir} = State) ->
    Paths = maps:get(Owner, State#state.files, []),
    if length(Paths) >= ?MAX_QUEUES_PER_PROCESS ->
	    {reply, {error, emfile}, State};
       true ->
	    Counter = State#state.counter + 1,
	    Path = filename:join(Dir, integer_to_list(Counter)),
	    {reply, {ok, Path}, State#state{counter = Counter}}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({monitor, Owner, Path}, State) ->
    Paths = maps:get(Owner, State#state.files, []),
    if Paths == [] -> erlang:monitor(process, Owner);
       true -> ok
    end,
    Files = maps:put(Owner, [Path|Paths], State#state.files),
    {noreply, State#state{files = Files}};
handle_cast({demonitor, Owner, Path}, State) ->
    spawn(fun() -> file:delete(Path) end),
    Paths = maps:get(Owner, State#state.files, []),
    Files = case lists:delete(Path, Paths) of
		[] ->
		    %% TODO: demonitor process
		    maps:remove(Owner, State#state.files);
		NewPaths ->
		    maps:put(Owner, NewPaths, State#state.files)
	    end,
    {noreply, State#state{files = Files}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, Owner, _Info}, State) ->
    Paths = maps:get(Owner, State#state.files, []),
    spawn(lists, foreach,
	  [fun(Path) ->
		   file:delete(Path)
	   end, Paths]),
    Files = maps:remove(Owner, State#state.files),
    {noreply, State#state{files = Files}};
handle_info(Info, State) ->
    error_logger:error_msg("unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{dir = Dir}) ->
    clean_dir(Dir).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_filename() ->
    p1_server:call(?MODULE, {get_filename, self()}).

clean_dir(Dir) ->
    filelib:fold_files(
      Dir, "[0-9]+", false,
      fun(File, _) -> file:delete(File) end,
      ok).

monitor_me(Path) ->
    p1_server:cast(?MODULE, {monitor, self(), Path}).

demonitor_me(Path) ->
    p1_server:cast(?MODULE, {demonitor, self(), Path}).

read_item_size(Fd, Pos) ->
    case file:pread(Fd, Pos, 4) of
	{ok, <<Size:32>>} ->
	    {ok, Size};
	{error, _} = Err ->
	    Err;
	_ ->
	    {error, corrupted}
    end.

read_item(Fd, Pos) ->
    case read_item_size(Fd, Pos) of
	{ok, Size} ->
	    case file:pread(Fd, Pos+4, Size) of
		{ok, Data} ->
		    try binary_to_term(Data) of
			Item -> {ok, Item, Pos + Size + 4}
		    catch _:_ ->
			    {error, corrupted}
		    end;
		{error, _} = Err ->
		    Err;
		_ ->
		    {error, corrupted}
	    end;
	{error, _} = Err ->
	    Err
    end.

to_list(_Fd, _Pos, 0, Items) ->
    lists:reverse(Items);
to_list(Fd, Pos, Len, Items) ->
    case read_item(Fd, Pos) of
	{ok, Item, NextPos} ->
	    to_list(Fd, NextPos, Len-1, [Item|Items]);
	{error, Err} ->
	    erlang:error(Err)
    end.

-define(MAX_HEAD, 1000).
%% @doc shrink head when there are more than MAX_HEAD elements in the head
gc(#file_q{fd = Fd, path = Path,
	   tail = Tail, head = Head,
	   start = Start, stop = Stop} = Q) ->
    if Head >= ?MAX_HEAD, Stop > Start ->
            try
                {ok, NewFd} = file:open(Path, [read, write, raw, binary]),
                {ok, _} = file:position(Fd, Start),
                {ok, _} = file:copy(Fd, NewFd, Stop - Start),
                file:close(Fd),
                {ok, _} = file:position(NewFd, Stop - Start),
                ok = file:truncate(NewFd),
                #file_q{fd = NewFd, start = 0, stop = Stop - Start,
                        head = 0, tail = Tail, path = Path}
            catch _:{badmatch, {error, Err}} ->
                    erlang:error(Err);
                  _:{badmatch, eof} ->
                    erlang:error(corrupted)
            end;
       true ->
            Q
    end.
