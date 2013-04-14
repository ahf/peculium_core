%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Inv Utilities.
-module(peculium_inv).

%% API.
-export([type/1, hash/1, is_transaction/1, is_block/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the type of a given inv.
-spec type(bitcoin_inv()) -> bitcoin_inv_atom().
type(#bitcoin_inv { type = Type }) ->
    Type.

%% @doc Returns the hash of a given inv.
-spec hash(bitcoin_inv()) -> binary().
hash(#bitcoin_inv { hash = Hash }) ->
    Hash.

%% @doc Checks if a given inv is a transaction.
-spec is_transaction(bitcoin_inv()) -> boolean().
is_transaction(Inv) ->
    type(Inv) =:= tx.

%% @doc Checks if a given inv is a block.
-spec is_block(bitcoin_inv()) -> boolean().
is_block(Inv) ->
    type(Inv) =:= block.
