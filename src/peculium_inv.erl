%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Inv Utilities.
-module(peculium_inv).

%% API.
-export([type/1, hash/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the type of a given inv.
-spec type(bitcoin_inv()) -> bitcoin_inv_atom().
type(#bitcoin_inv { type = Type }) ->
    Type.

%% @doc Returns the hash of a given inv.
-spec hash(bitcoin_inv()) -> binary().
hash(#bitcoin_inv { hash = Hash }) ->
    Hash.
