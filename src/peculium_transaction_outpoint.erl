%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Outpoint Utilities.
-module(peculium_transaction_outpoint).

%% API.
-export([index/1, hash/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the index of a given outpoint.
-spec index(Outpoint :: bitcoin_transaction_outpoint()) -> uint32_t().
index(#bitcoin_transaction_outpoint { index = Index }) ->
    Index.

%% @doc Returns the hash of a given outpoint.
-spec hash(Outpoint :: bitcoin_transaction_outpoint()) -> binary().
hash(#bitcoin_transaction_outpoint { hash = Hash }) ->
    Hash.
