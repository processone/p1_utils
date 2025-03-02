%%%----------------------------------------------------------------------
%%% File    : p1_rand.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Random generation number wrapper
%%% Created : 13 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2025 ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(p1_rand).

-author('alexey@process-one.net').

-export([get_string/0, uniform/0, uniform/1, uniform/2, bytes/1,
	 round_robin/1, get_alphanum_string/1]).

-define(THRESHOLD, 16#10000000000000000).

-ifdef(HAVE_RAND).
get_string() ->
    R = rand:uniform(?THRESHOLD),
    integer_to_binary(R).

uniform() ->
    rand:uniform().

uniform(N) ->
    rand:uniform(N).

uniform(N, M) ->
    rand:uniform(M-N+1) + N-1.
-else.
get_string() ->
    R = crypto:rand_uniform(0, ?THRESHOLD),
    integer_to_binary(R).

uniform() ->
    crypto:rand_uniform(0, ?THRESHOLD)/?THRESHOLD.

uniform(N) ->
    crypto:rand_uniform(1, N+1).

uniform(N, M) ->
    crypto:rand_uniform(N, M+1).
-endif.

-spec bytes(non_neg_integer()) -> binary().
bytes(N) ->
    crypto:strong_rand_bytes(N).

-spec round_robin(pos_integer()) -> non_neg_integer().
round_robin(N) ->
    p1_time_compat:unique_integer([monotonic, positive]) rem N.

-spec get_alphanum_string(non_neg_integer()) -> binary().
get_alphanum_string(Length) ->
    list_to_binary(get_alphanum_string([], Length)).

-spec get_alphanum_string(string(), non_neg_integer()) -> string().
get_alphanum_string(S, 0) -> S;
get_alphanum_string(S, N) ->
    get_alphanum_string([make_rand_char() | S], N - 1).

-spec make_rand_char() -> char().
make_rand_char() ->
    map_int_to_char(uniform(0, 61)).

-spec map_int_to_char(0..61) -> char().
map_int_to_char(N) when N =<  9 -> N + 48; % Digit.
map_int_to_char(N) when N =< 35 -> N + 55; % Upper-case character.
map_int_to_char(N) when N =< 61 -> N + 61. % Lower-case character.
