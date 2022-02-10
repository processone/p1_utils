%%%----------------------------------------------------------------------
%%% File    : p1_proxy_protocol.erl
%%% Author  : Paweł Chmielowski <pawel@process-one.net>
%%% Purpose :
%%% Created : 27 Nov 2018 by Paweł Chmielowski <pawel@process-one.net>
%%%
%%%
%%% Copyright (C) 2002-2022 ProcessOne, SARL. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%----------------------------------------------------------------------
-module(p1_proxy_protocol).
-author("pawel@process-one.net").

%% API
-export([decode/3]).

-spec decode(gen_tcp | ssl, inet:socket(), integer())
      -> {{inet:ip_address(), inet:port_number()},
	  {inet:ip_address(), inet:port_number()}}
	 | {error, atom()}
	 | {undefined, undefined}.
decode(SockMod, Socket, Timeout) ->
    V = SockMod:recv(Socket, 6, Timeout),
    case V of
	{ok, <<"PROXY ">>} ->
	    decode_v1(SockMod, Socket, Timeout);
	{ok, <<16#0d, 16#0a, 16#0d, 16#0a, 16#00, 16#0d>>} ->
	    decode_v2(SockMod, Socket, Timeout);
	_ ->
	    {error, eproto}
    end.

decode_v1(SockMod, Socket, Timeout) ->
    case read_until_rn(SockMod, Socket, <<>>, false, Timeout) of
	{error, _} = Err ->
	    Err;
	Val ->
	    case binary:split(Val, <<" ">>, [global]) of
		[<<"TCP4">>, SAddr, DAddr, SPort, DPort] ->
		    try {inet_parse:ipv4strict_address(binary_to_list(SAddr)),
			 inet_parse:ipv4strict_address(binary_to_list(DAddr)),
			 binary_to_integer(SPort),
			 binary_to_integer(DPort)}
		    of
			{{ok, DA}, {ok, SA}, DP, SP} ->
			    {{SA, SP}, {DA, DP}};
			_ ->
			    {error, eproto}
		    catch
			error:badarg ->
			    {error, eproto}
		    end;
		[<<"TCP6">>, SAddr, DAddr, SPort, DPort] ->
		    try {inet_parse:ipv6strict_address(binary_to_list(SAddr)),
			 inet_parse:ipv6strict_address(binary_to_list(DAddr)),
			 binary_to_integer(SPort),
			 binary_to_integer(DPort)}
		    of
			{{ok, DA}, {ok, SA}, DP, SP} ->
			    {{SA, SP}, {DA, DP}};
			_ ->
			    {error, eproto}
		    catch
			error:badarg ->
			    {error, eproto}
		    end;
		[<<"UNKNOWN">> | _] ->
		    {undefined, undefined}
	    end
    end.

decode_v2(SockMod, Socket, Timeout) ->
    case SockMod:recv(Socket, 10, Timeout) of
	{error, _} = Err ->
	    Err;
	{ok, <<16#0a, 16#51, 16#55, 16#49, 16#54, 16#0a,
	       2:4, Command:4, Transport:8, AddrLen:16/big-unsigned-integer>>} ->
	    case SockMod:recv(Socket, AddrLen, Timeout) of
		{error, _} = Err ->
		    Err;
		{ok, Data} ->
		    case Command of
			0 ->
			    case {inet:sockname(Socket), inet:peername(Socket)} of
				{{ok, SA}, {ok, DA}} ->
				    {SA, DA};
				{{error, _} = E, _} ->
				    E;
				{_, {error, _} = E} ->
				    E
			    end;
			1 ->
			    case Transport of
				% UNSPEC or UNIX
				V when V == 0; V == 16#31; V == 16#32 ->
				    {{unknown, unknown}, {unknown, unknown}};
				% IPV4 over TCP or UDP
				V when V == 16#11; V == 16#12 ->
				    case Data of
					<<D1:8, D2:8, D3:8, D4:8,
					  S1:8, S2:8, S3:8, S4:8,
					  DP:16/big-unsigned-integer,
					  SP:16/big-unsigned-integer,
					  _/binary>> ->
					    {{{S1, S2, S3, S4}, SP},
					     {{D1, D2, D3, D4}, DP}};
					_ ->
					    {error, eproto}
				    end;
				% IPV6 over TCP or UDP
				V when V == 16#21; V == 16#22 ->
				    case Data of
					<<D1:16/big-unsigned-integer,
					  D2:16/big-unsigned-integer,
					  D3:16/big-unsigned-integer,
					  D4:16/big-unsigned-integer,
					  D5:16/big-unsigned-integer,
					  D6:16/big-unsigned-integer,
					  D7:16/big-unsigned-integer,
					  D8:16/big-unsigned-integer,
					  S1:16/big-unsigned-integer,
					  S2:16/big-unsigned-integer,
					  S3:16/big-unsigned-integer,
					  S4:16/big-unsigned-integer,
					  S5:16/big-unsigned-integer,
					  S6:16/big-unsigned-integer,
					  S7:16/big-unsigned-integer,
					  S8:16/big-unsigned-integer,
					  DP:16/big-unsigned-integer,
					  SP:16/big-unsigned-integer,
					  _/binary>> ->
					    {{{S1, S2, S3, S4, S5, S6, S7, S8}, SP},
					     {{D1, D2, D3, D4, D5, D6, D7, D8}, DP}};
					_ ->
					    {error, eproto}
				    end
			    end;
			_ ->
			    {error, eproto}
		    end
	    end;
	<<16#0a, 16#51, 16#55, 16#49, 16#54, 16#0a, _/binary>> ->
	    {error, eproto};
	_ ->
	    {error, eproto}
    end.

read_until_rn(_SockMod, _Socket, Data, _, _) when size(Data) > 107 ->
    {error, eproto};
read_until_rn(SockMod, Socket, Data, true, Timeout) ->
    case SockMod:recv(Socket, 1, Timeout) of
	{ok, <<"\n">>} ->
	    Data;
	{ok, <<"\r">>} ->
	    read_until_rn(SockMod, Socket, <<Data/binary, "\r">>,
			  true, Timeout);
	{ok, Other} ->
	    read_until_rn(SockMod, Socket, <<Data/binary, "\r", Other/binary>>,
			  false, Timeout);
	{error, _} = Err ->
	    Err
    end;
read_until_rn(SockMod, Socket, Data, false, Timeout) ->
    case SockMod:recv(Socket, 2, Timeout) of
	{ok, <<"\r\n">>} ->
	    Data;
	{ok, <<Byte:8, "\r">>} ->
	    read_until_rn(SockMod, Socket, <<Data/binary, Byte:8>>,
			  true, Timeout);
	{ok, Other} ->
	    read_until_rn(SockMod, Socket, <<Data/binary, Other/binary>>,
			  false, Timeout);
	{error, _} = Err ->
	    Err
    end.
