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
%%% @doc Bitcoin Block Header Utilities.
%%% This module contains utilities for manipulating and using Block Header
%%% objects.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_block_header).

%% API.
-export([from_block/1, version/1, previous/1, merkle_root/1, timestamp/1,
        bits/1, nonce/1, transaction_count/1, difficulty/1, block_work/1]).

%% Types.
-type hash() :: peculium_types:hash().
-type block() :: peculium_types:block().
-type block_header() :: peculium_types:block_header().
-type uint8_t() :: peculium_types:uint8_t().
-type uint32_t() :: peculium_types:uint32_t().

-include_lib("peculium/include/peculium.hrl").

%% @doc Create Bitcoin Block Header from a given Block.
-spec from_block(Block :: block()) -> block_header().
from_block(#block { version = Version, previous_block = PreviousBlock, merkle_root = MerkleRoot, timestamp = Timestamp, bits = Bits, nonce = Nonce }) ->
    #block_header {
        version = Version,
        previous_block = PreviousBlock,
        merkle_root = MerkleRoot,
        timestamp = Timestamp,
        bits = Bits,
        nonce = Nonce,
        transaction_count = 0
    }.

%% @doc Returns the version of a given block header.
-spec version(BlockHeader :: block_header()) -> uint32_t().
version(#block_header { version = Version }) ->
    Version.

%% @doc Returns the hash of the previous block of a given block header.
-spec previous(BlockHeader :: block_header()) -> hash().
previous(#block_header { previous_block = PreviousBlock }) ->
    peculium_utilities:reverse(PreviousBlock).

%% @doc Returns the root hash of the merkle tree of a given block header.
-spec merkle_root(BlockHeader :: block_header()) -> hash().
merkle_root(#block_header { merkle_root = MerkleRoot }) ->
    peculium_utilities:reverse(MerkleRoot).

%% @doc Returns the timestamp of a given block header.
-spec timestamp(BlockHeader :: block_header()) -> uint32_t().
timestamp(#block_header { timestamp = Timestamp }) ->
    Timestamp.

%% @doc Returns the bits of a given block header.
-spec bits(BlockHeader :: block_header()) -> uint32_t().
bits(#block_header { bits = Bits }) ->
    Bits.

%% @doc Returns the nonce of a given block header.
-spec nonce(Blockheader :: block_header()) -> uint32_t().
nonce(#block_header { nonce = Nonce }) ->
    Nonce.

%% @doc Returns the transaction count (always 0) of a given block header.
-spec transaction_count(BlockHeader :: block_header()) -> uint8_t().
transaction_count(#block_header { transaction_count = TransactionCount }) ->
    TransactionCount.

%% @doc Returns the difficulty for the given block header.
-spec difficulty(BlockHeader :: block_header()) -> number().
difficulty(#block_header { bits = Bits }) ->
    peculium_difficulty:from_bits(Bits).

%% @doc Returns the block work for the given block header.
-spec block_work(BlockHeader :: block_header()) -> number().
block_work(#block_header { bits = Bits }) ->
    peculium_difficulty:block_work(Bits).
