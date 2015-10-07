%%%-------------------------------------------------------------------
%%% File    : p1_nif_utils.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Description : Helper utilities for handling nif code
%%%
%%% Created : 7 Oct 2015 by Paweł Chmielowski <pawel@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------
-module(p1_nif_utils).

-export([get_so_path/3]).

get_so_path(ModuleName, AppNames, SoName) ->
    PrivDir = first_match(fun(App) ->
                                  case code:priv_dir(App) of
                                      {error, _} -> none;
                                      V -> V
                                  end
                          end, AppNames),
    case PrivDir of
        none ->
            Ext = case os:type() of
                      {win32, _} -> ".dll";
                      _ -> ".so"
                  end,
            SoFName = filename:join(["priv", "lib", SoName ++ Ext]),
            LPath = first_match(fun(Path) ->
                                        P = case filename:basename(Path) of
                                                ebin -> filename:dirname(Path);
                                                _ -> Path
                                            end,
                                        case filelib:is_file(filename:join([P, SoFName])) of
                                            true ->
                                                filename:join([P, "priv", "lib", SoName]);
                                    _ ->
                                                none
                                        end
                                end, code:get_path()),
            case LPath of
                none ->
                    EbinDir = filename:dirname(code:which(ModuleName)),
                    AppDir = filename:dirname(EbinDir),
                    filename:join([AppDir, "priv", "lib", SoName]);
                Val ->
                    Val
            end;
        V ->
            filename:join([V, "lib", SoName])
    end.

first_match(_Fun, []) ->
    none;
first_match(Fun, [H|T]) ->
    case Fun(H) of
        none ->
            first_match(Fun, T);
        V  ->
            V
    end.
