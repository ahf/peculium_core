%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Outpoint Utilities.
-module(peculium_outpoint).

%% API.
-export([index/1, hash/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the index of a given outpoint.
-spec index(Outpoint :: bitcoin_outpoint()) -> uint32_t().
index(#bitcoin_outpoint { index = Index }) ->
    Index.

%% @doc Returns the hash of a given outpoint.
-spec hash(Outpoint :: bitcoin_outpoint()) -> binary().
hash(#bitcoin_outpoint { hash = Hash }) ->
    Hash.
