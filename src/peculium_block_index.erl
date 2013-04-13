%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Index Types and Utilities.
-module(peculium_block_index).

%% API.
-export([new/4, from_block/1, hash/1, previous/1, previous_index/1, next/1, next_index/1, height/1, block/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Creates a new block index entry.
-spec new(Hash :: binary(), Height :: non_neg_integer(), Previous :: binary(), Next :: binary()) -> block_index_entry().
new(Hash, Height, Previous, Next) ->
    #block_index_entry {
        hash = Hash,
        height = Height,
        previous = Previous,
        next = Next
    }.

%% @doc Create a new block index entry from a Block.
-spec from_block(Block :: bitcoin_block_message()) -> block_index_entry().
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
            peculium_block_index_srv:get(PreviousBlockHash)
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
            peculium_block_index_srv:get(NextBlockHash)
    end.

%% @doc Returns the height of a given block index entry.
-spec height(block_index_entry()) -> non_neg_integer().
height(#block_index_entry { height = Height }) ->
    Height.

%% @doc Returns the block of a given block index entry.
-spec block(block_index_entry()) -> bitcoin_block_message().
block(#block_index_entry { hash = Hash }) ->
    {ok, Block} = peculium_block_store_srv:get(Hash),
    Block.
