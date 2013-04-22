%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions. All rights reserved.
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
%%% @doc        Bitcoin Block Index Types and Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_block_index_entry).

%% API.
-export([from_block/1, hash/1, previous/1, previous_index/1, next/1, next_index/1, height/1, block/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Create a new block index entry from a Block.
-spec from_block(Block :: bitcoin_block()) -> block_index_entry().
from_block(Block) ->
    #block_index_entry {
        hash = peculium_block:hash(Block),
        height = undefined,
        previous = peculium_block:previous(Block),
        next = undefined
    }.

%% @doc Returns the hash of the block that the given block index entry points to.
-spec hash(block_index_entry()) -> binary().
hash(#block_index_entry { hash = Hash }) ->
    Hash.

%% @doc Returns the hash of the previous block of a given block index entry.
-spec previous(block_index_entry()) -> binary() | undefined.
previous(#block_index_entry { previous = Previous }) ->
    Previous.

%% @doc Returns the block index entry of the previous block of a given block index entry.
-spec previous_index(block_index_entry()) -> block_index_entry() | undefined.
previous_index(BlockIndexEntry) ->
    case previous(BlockIndexEntry) of
        undefined ->
            undefined;
        PreviousBlockHash ->
            peculium_block_index:get(PreviousBlockHash)
    end.

%% @doc Returns the hash of the next block of a given block index entry.
-spec next(block_index_entry()) -> binary() | undefined.
next(#block_index_entry { next = Next }) ->
    Next.

%% @doc Returns the block index entry of the next block of a given block index entry.
-spec next_index(block_index_entry()) -> block_index_entry() | undefined.
next_index(BlockIndexEntry) ->
    case next(BlockIndexEntry) of
        undefined ->
            undefined;
        NextBlockHash ->
            peculium_block_index:get(NextBlockHash)
    end.

%% @doc Returns the height of a given block index entry.
-spec height(block_index_entry()) -> non_neg_integer().
height(#block_index_entry { height = Height }) ->
    Height.

%% @doc Returns the block of a given block index entry.
-spec block(block_index_entry()) -> bitcoin_block().
block(#block_index_entry { hash = Hash }) ->
    {ok, Block} = peculium_block_store:get(Hash),
    Block.
