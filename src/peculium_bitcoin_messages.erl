%% Copyright (c) 2013 Alexander Færøy
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(peculium_bitcoin_messages).

-export([verack/1, getaddr/1, ping/1, version/3, getdata/2, getblocks/3, getheaders/3]).

-include_lib("peculium/include/peculium.hrl").
-include_lib("erl_aliases/include/erl_aliases.hrl").

-module_alias({t, peculium_bitcoin_protocol_types}).
-module_alias({u, peculium_bitcoin_protocol_utilities}).

pad_command(Command) ->
    X = atom_to_binary(Command, utf8),
    <<X/binary, 0:((12 - size(X)) * 8)>>.

encode(Network, Command) ->
    encode(Network, Command, []).

encode(Network, Command, Payload) ->
    {ok, MagicValue} = peculium_bitcoin_network:magic_value(Network),
    encode(MagicValue, pad_command(Command), iolist_size(Payload), u:checksum(Payload), Payload).

encode(MagicValue, Command, PayloadSize, Checksum, Payload) ->
    [MagicValue, Command, t:uint32_t(PayloadSize), Checksum, Payload].

verack(Network) ->
    encode(Network, verack).

getaddr(Network) ->
    encode(Network, getaddr).

ping(Network) ->
    encode(Network, ping).

version(Network, {SourceAddress, SourcePort}, {DestinationAddress, DestinationPort}) ->
    encode(Network, version, [t:int32_t(60001), t:uint64_t(1), t:uint64_t(peculium_utilities:timestamp()), t:net_addr(DestinationAddress, DestinationPort), t:net_addr(SourceAddress, SourcePort), crypto:rand_bytes(8), t:var_string("Peculium"), t:int32_t(0)]).

getdata(Network, Invs) ->
    {ok, Length} = t:var_int(length(Invs)),
    encode(Network, getdata, [Length, lists:map(fun peculium_bitcoin_protocol_types:inv/1, Invs)]).

getblocks(Network, BlockLocator, BlockStop) ->
    {ok, Length} = t:var_int(length(BlockLocator)),
    encode(Network, getblocks, [t:int32_t(60001), Length, BlockLocator, BlockStop]).

getheaders(Network, BlockLocator, BlockStop) ->
    {ok, Length} = t:var_int(length(BlockLocator)),
    encode(Network, getheaders, [t:int32_t(60001), Length, BlockLocator, BlockStop]).
