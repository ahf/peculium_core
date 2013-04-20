%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Crypto Utilities.
-module(peculium_crypto).

%% API.
-export([hash/1]).

%% @doc Returns the double SHA256 checksum of a given input.
-spec hash(iolist()) -> binary().
hash(X) ->
    peculium_utilities:reverse(crypto:sha256(crypto:sha256(X))).
