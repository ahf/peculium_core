%%%
%%% Copyright (c) 2013 Alexander Færøy.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% ----------------------------------------------------------------------------
%%% @author     Alexander Færøy <ahf@0x90.dk>
%%% @doc        Bitcoin protocol message encoders.
%%% ----------------------------------------------------------------------------
-module(peculium_core_messages).

-export([verack/1, getaddr/1, ping/1, version/1, getdata/1, getblocks/1, getheaders/1, block/1]).

pad_command(Command) ->
    X = atom_to_binary(Command, utf8),
    <<X/binary, 0:((12 - size(X)) * 8)>>.

encode(Network, Command) ->
    encode(Network, Command, []).

encode(Network, Command, Payload) ->
    {ok, MagicValue} = peculium_core_network:magic_value(Network),
    encode(MagicValue, pad_command(Command), iolist_size(Payload), peculium_core_protocol_utilities:checksum(Payload), Payload).

encode(MagicValue, Command, PayloadSize, Checksum, Payload) ->
    [MagicValue, Command, peculium_core_protocol_types:uint32_t(PayloadSize), Checksum, Payload].

verack([Network]) ->
    encode(Network, verack).

getaddr([Network]) ->
    encode(Network, getaddr).

ping([Network]) ->
    encode(Network, ping).

version([Network, {SourceAddress, SourcePort}, {DestinationAddress, DestinationPort}]) ->
    {ok, BlockHeight} = peculium_core_block_index:best_block_height(),
    encode(Network, version, [peculium_core_protocol_types:int32_t(60001), peculium_core_protocol_types:uint64_t(1), peculium_core_protocol_types:uint64_t(peculium_core_utilities:timestamp()), peculium_core_protocol_types:net_addr(DestinationAddress, DestinationPort), peculium_core_protocol_types:net_addr(SourceAddress, SourcePort), crypto:rand_bytes(8), peculium_core_protocol_types:var_string(peculium:user_agent()), peculium_core_protocol_types:int32_t(BlockHeight)]).

getdata([Network, Invs]) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(Invs)),
    Data = lists:map(fun peculium_core_protocol_types:inv/1, Invs),
    encode(Network, getdata, [Length, Data]).

getblocks([Network, BlockLocator, BlockStop]) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(BlockLocator)),
    encode(Network, getblocks, [peculium_core_protocol_types:int32_t(60001), Length, BlockLocator, BlockStop]).

getheaders([Network, BlockLocator, BlockStop]) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(BlockLocator)),
    encode(Network, getheaders, [peculium_core_protocol_types:int32_t(60001), Length, BlockLocator, BlockStop]).

block([Network, Version, PreviousBlock, MerkleRoot, Timestamp, Bits, Nonce, Transactions]) ->
    encode(Network, block, [peculium_core_protocol_types:uint32_t(Version), PreviousBlock, MerkleRoot, peculium_core_protocol_types:uint32_t(Timestamp), peculium_core_protocol_types:uint32_t(Bits), peculium_core_protocol_types:uint32_t(Nonce), lists:map(fun peculium_core_protocol_types:transaction/1, Transactions)]).
