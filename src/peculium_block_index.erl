%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Index Types and Utilities.
-module(peculium_block_index).

%% API.
-export([new/4, hash/1, previous/1, next/1, height/1]).

-record(block_index_entry, {
    hash :: binary(),
    height :: non_neg_integer(),
    previous = undefined :: undefined | binary(),
    next = undefined :: undefined | binary()
}).

-type block_index_entry() :: #block_index_entry {}.

%% @doc Creates a new block index entry.
-spec new(Hash :: binary(), Height :: non_neg_integer(), Previous :: binary(), Next :: binary()) -> block_index_entry().
new(Hash, Height, Previous, Next) ->
    #block_index_entry {
        hash = Hash,
        height = Height,
        previous = Previous,
        next = Next
    }.

%% @doc Returns the hash of the block that the given block index entry points to.
-spec hash(block_index_entry()) -> binary().
hash(#block_index_entry { hash = Hash }) ->
    Hash.

%% @doc Returns the hash of the previous block of a given block index entry.
-spec previous(block_index_entry()) -> binary() | undefined.
previous(#block_index_entry { previous = Previous }) ->
    Previous.

%% @doc Returns the hash of the next block of a given block index entry.
-spec next(block_index_entry()) -> binary() | undefined.
next(#block_index_entry { next = Next }) ->
    Next.

%% @doc Returns the height of a given block index entry.
-spec height(block_index_entry()) -> non_neg_integer().
height(#block_index_entry { height = Height }) ->
    Height.
