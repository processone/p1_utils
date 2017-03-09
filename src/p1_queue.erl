%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% @copyright (C) 2017, Evgeny Khramtsov
%%% @doc
%%%
%%% @end
%%% Created :  8 Mar 2017 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%-------------------------------------------------------------------
-module(p1_queue).

%% API
-export([new/0, new/1, is_queue/1, len/1, is_empty/1, in/2, out/1,
	 peek/1, drop/1, from_list/1, from_list/2, to_list/1, clear/1,
	 foreach/2, foldl/3, dropwhile/2, type/1, format_error/1]).
-export([start/1, stop/0]).

-type rqueue() :: {queue:queue(), non_neg_integer()}.
-type fqueue() :: p1_file_queue:queue().
-type queue() :: rqueue() | fqueue().
-type queue_type() :: ram | file.
-export_type([queue/0, queue_type/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start(filename:filename()) -> ok | {error, any()}.
start(Dir) ->
    application:ensure_all_started(p1_utils),
    case p1_file_queue:start(Dir) of
	{ok, _} -> ok;
	{error, {already_started, _}} -> ok;
	Err -> Err
    end.

-spec stop() -> ok | {error, any()}.
stop() ->
    p1_file_queue:stop().

-spec new() -> rqueue().
new() ->
    new(ram).

-spec new(ram) -> rqueue();
	 (file) -> fqueue().
new(ram) ->
    {queue:new(), 0};
new(file) ->
    p1_file_queue:new().

-spec type(queue()) -> ram | {file, filename:filename()}.
type({_, _}) ->
    ram;
type(Q) ->
    {file, p1_file_queue:path(Q)}.

-spec is_queue(any()) -> boolean().
is_queue({Q, Len}) when is_integer(Len), Len >= 0 ->
    queue:is_queue(Q);
is_queue(Q) ->
    p1_file_queue:is_queue(Q).

-spec len(queue()) -> non_neg_integer().
len({_, Len}) ->
    Len;
len(Q) ->
    p1_file_queue:len(Q).

-spec is_empty(queue()) -> boolean().
is_empty({_, Len}) ->
    Len == 0;
is_empty(Q) ->
    p1_file_queue:is_empty(Q).

-spec in(term(), rqueue()) -> rqueue();
	(term(), fqueue()) -> fqueue().
in(Item, {Q, Len}) ->
    {queue:in(Item, Q), Len+1};
in(Item, Q) ->
    p1_file_queue:in(Item, Q).

-spec out(rqueue()) -> {{value, term()}, rqueue()} | {empty, rqueue()};
	 (fqueue()) -> {{value, term()}, fqueue()} | {empty, fqueue()}.
out({Q, 0}) ->
    {empty, {Q, 0}};
out({Q, Len}) ->
    {{value, Item}, Q1} = queue:out(Q),
    {{value, Item}, {Q1, Len-1}};
out(Q) ->
    p1_file_queue:out(Q).

-spec peek(queue()) -> empty | {value, term()}.
peek({Q, _}) ->
    queue:peek(Q);
peek(Q) ->
    p1_file_queue:peek(Q).

-spec drop(rqueue()) -> rqueue();
	  (fqueue()) -> fqueue().
drop({Q, Len}) ->
    {queue:drop(Q), Len-1};
drop(Q) ->
    p1_file_queue:drop(Q).

-spec from_list(list()) -> rqueue().
from_list(L) ->
    from_list(L, ram).

-spec from_list(list(), ram) -> rqueue();
	       (list(), file) -> fqueue().
from_list(L, ram) ->
    {queue:from_list(L), length(L)};
from_list(L, file) ->
    p1_file_queue:from_list(L).

-spec to_list(queue()) -> list().
to_list({Q, _}) ->
    queue:to_list(Q);
to_list(Q) ->
    p1_file_queue:to_list(Q).

-spec foreach(fun((term()) -> term()), fqueue()) -> ok.
foreach(F, {Q, Len}) ->
    case queue:out(Q) of
	{{value, Item}, Q1} ->
	    F(Item),
	    foreach(F, {Q1, Len-1});
	{empty, _} ->
	    ok
    end;
foreach(F, Q) ->
    p1_file_queue:foreach(F, Q).

-spec foldl(fun((term(), T) -> T), T, queue()) -> T.
foldl(F, Acc, {Q, Len}) ->
    case queue:out(Q) of
	{{value, Item}, Q1} ->
	    Acc1 = F(Item, Acc),
	    foldl(F, Acc1, {Q1, Len-1});
	{empty, _} ->
	    Acc
    end;
foldl(F, Acc, Q) ->
    p1_file_queue:foldl(F, Acc, Q).

-spec dropwhile(fun((term()) -> boolean()), rqueue()) -> rqueue();
		(fun((term()) -> boolean()), fqueue()) -> fqueue().
dropwhile(_, {_, 0} = Q) ->
    Q;
dropwhile(F, {Q, Len}) ->
    {value, Item} = queue:peek(Q),
    case F(Item) of
	true ->
	    dropwhile(F, {queue:drop(Q), Len-1});
	_ ->
	    {Q, Len}
    end;
dropwhile(F, Q) ->
    p1_file_queue:dropwhile(F, Q).

-spec clear(rqueue()) -> rqueue();
	   (fqueue()) -> fqueue().
clear({_, _}) ->
    {queue:new(), 0};
clear(Q) ->
    p1_file_queue:clear(Q).

format_error(Reason) ->
    p1_file_queue:format_error(Reason).

%%%===================================================================
%%% Internal functions
%%%===================================================================
