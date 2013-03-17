-module(peculium_bitcoin_network).

-export([magic_value/1, stringify/1]).

-include_lib("peculium/include/peculium.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec magic_value(bitcoin_network_atom()) -> {ok, binary()} | {error, {invalid_network, any()}}.
magic_value(mainnet) ->
    {ok, binary:encode_unsigned(16#D9B4BEF9, little)};
magic_value(testnet) ->
    {ok, binary:encode_unsigned(16#DAB5BFFA, little)};
magic_value(testnet3) ->
    {ok, binary:encode_unsigned(16#0709110B, little)};
magic_value(X) ->
    {error, {invalid_network, X}}.

-spec stringify(bitcoin_network_atom()) -> {ok, binary()} | {error, {invalid_network, any()}}.
stringify(mainnet) ->
    {ok, <<"mainnet">>};
stringify(testnet) ->
    {ok, <<"testnet">>};
stringify(testnet3) ->
    {ok, <<"testnet3">>};
stringify(X) ->
    {error, {invalid_network, X}}.

-ifdef(TEST).

-spec test() -> any().

-spec magic_value_test() -> any().
magic_value_test() ->
    ?assertEqual(magic_value(mainnet), {ok, <<16#F9, 16#BE, 16#B4, 16#D9>>}),
    ?assertEqual(magic_value(testnet), {ok, <<16#FA, 16#BF, 16#B5, 16#DA>>}),
    ?assertEqual(magic_value(testnet3), {ok, <<16#0B, 16#11, 16#09, 16#07>>}),
    ?assertEqual(magic_value(dongs), {error, {invalid_network, dongs}}),
    ?assertEqual(magic_value(1337), {error, {invalid_network, 1337}}).

-spec stringify_test() -> any().
stringify_test() ->
    ?assertEqual(stringify(mainnet), {ok, <<"mainnet">>}),
    ?assertEqual(stringify(testnet), {ok, <<"testnet">>}),
    ?assertEqual(stringify(testnet3), {ok, <<"testnet3">>}),
    ?assertEqual(stringify(1337), {error, {invalid_network, 1337}}).

-endif.
