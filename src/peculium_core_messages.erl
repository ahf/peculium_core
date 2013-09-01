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
%%% @copyright  2013 Alexander Færøy
%%% @end
%%% ----------------------------------------------------------------------------
%%% @doc Bitcoin message encoders
%%%
%%% This module is used by the peculium_core_peer process to create Bitcoin
%%% messages.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_messages).

%% API.
-export([verack/1, getaddr/1, ping/1, version/6, getdata/2, getblocks/3,
        getheaders/3, block/8]).

%% Types.
-type block_locator() :: peculium_core_types:block_locator().
-type command() :: peculium_core_types:command().
-type hash() :: peculium_core_types:hash().
-type inv() :: peculium_core_types:inv().
-type network() :: peculium_core_types:network().
-type transaction() :: peculium_core_types:transaction().
-type uint32_t() :: peculium_core_types:uint32_t().

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Create verack message in the network wire format.
-spec verack(Network :: network()) -> iolist().
verack(Network) ->
    encode(Network, verack).

%% @doc Create getaddr message in the network wire format.
-spec getaddr(Network :: network()) -> iolist().
getaddr(Network) ->
    encode(Network, getaddr).

%% @doc Create ping message in the network wire format.
-spec ping(Network :: network()) -> iolist().
ping(Network) ->
    encode(Network, ping).

%% @doc Create version message in the network wire format.
-spec version(Network :: network(), SourceAddress :: inet:ip_address(), SourcePort :: inet:port_number(), DestinationAddress :: inet:ip_address(), DestinationPort :: inet:port_number(), Nonce :: binary()) -> iolist().
version(Network, SourceAddress, SourcePort, DestinationAddress, DestinationPort, Nonce) ->
    {ok, BlockHeight} = peculium_core_block_index:best_block_height(),
    encode(Network, version, [peculium_core_protocol_types:int32_t(60001), peculium_core_protocol_types:uint64_t(1), peculium_core_protocol_types:uint64_t(peculium_core_utilities:timestamp()), peculium_core_protocol_types:net_addr(DestinationAddress, DestinationPort), peculium_core_protocol_types:net_addr(SourceAddress, SourcePort), Nonce, peculium_core_protocol_types:var_string(peculium_core:user_agent()), peculium_core_protocol_types:int32_t(BlockHeight)]).

%% @doc Create getdata message in the network wire format.
-spec getdata(Network :: network(), Invs :: [inv()]) -> iolist().
getdata(Network, Invs) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(Invs)),
    Data = lists:map(fun peculium_core_protocol_types:inv/1, Invs),
    encode(Network, getdata, [Length, Data]).

%% @doc Create getblocks message in the network wire format.
-spec getblocks(Network :: network(), BlockLocator :: block_locator(), BlockStop :: hash()) -> iolist().
getblocks(Network, BlockLocator, BlockStop) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(BlockLocator)),
    encode(Network, getblocks, [peculium_core_protocol_types:int32_t(60001), Length, BlockLocator, BlockStop]).

%% @doc Create getheaders message in the network wire format.
-spec getheaders(Network :: network(), BlockLocator :: block_locator(), BlockStop :: hash()) -> iolist().
getheaders(Network, BlockLocator, BlockStop) ->
    {ok, Length} = peculium_core_protocol_types:var_int(length(BlockLocator)),
    encode(Network, getheaders, [peculium_core_protocol_types:int32_t(60001), Length, BlockLocator, BlockStop]).

%% @doc Create block message in the network wire format.
-spec block(Network :: network(), Version :: uint32_t(), PreviousBlock :: hash(), MerkleRoot :: hash(), Timestamp :: non_neg_integer(), Bits :: binary(), Nonce :: binary(), Transactions :: [transaction()]) -> iolist().
block(Network, Version, PreviousBlock, MerkleRoot, Timestamp, Bits, Nonce, Transactions) ->
    encode(Network, block, [peculium_core_protocol_types:uint32_t(Version), PreviousBlock, MerkleRoot, peculium_core_protocol_types:uint32_t(Timestamp), peculium_core_protocol_types:uint32_t(Bits), peculium_core_protocol_types:uint32_t(Nonce), lists:map(fun peculium_core_protocol_types:transaction/1, Transactions)]).

%% @private
-spec pad_command(Command :: command()) -> binary().
pad_command(Command) ->
    X = atom_to_binary(Command, utf8),
    <<X/binary, 0:((12 - size(X)) * 8)>>.

%% @private
-spec encode(Network :: network(), Command :: command()) -> iolist().
encode(Network, Command) ->
    encode(Network, Command, []).

%% @private
-spec encode(Network :: network(), Command :: command(), Payload :: iolist()) -> iolist().
encode(Network, Command, Payload) ->
    {ok, MagicValue} = peculium_core_network:magic_value(Network),
    encode(MagicValue, pad_command(Command), iolist_size(Payload), peculium_core_protocol_utilities:checksum(Payload), Payload).

%% @private
-spec encode(MagicValue :: binary(), Command :: binary(), PayloadSize :: uint32_t(), Checksum :: binary(), Payload :: iolist()) -> iolist().
encode(MagicValue, Command, PayloadSize, Checksum, Payload) ->
    [MagicValue, Command, peculium_core_protocol_types:uint32_t(PayloadSize), Checksum, Payload].
