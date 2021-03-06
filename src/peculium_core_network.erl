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
%%% @doc Bitcoin Network Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_network).

%% API.
-export([magic_value/1, port_number/1, stringify/1]).

%% Types.
-type network() :: peculium_core_types:network().

-include("peculium_core.hrl").

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Get network wire magical value from a given network.
-spec magic_value(Network :: network()) -> {ok, binary()} | {error, term()}.
magic_value(mainnet) ->
    {ok, binary:encode_unsigned(16#D9B4BEF9, little)};
magic_value(testnet) ->
    {ok, binary:encode_unsigned(16#DAB5BFFA, little)};
magic_value(testnet3) ->
    {ok, binary:encode_unsigned(16#0709110B, little)};
magic_value(X) ->
    {error, {invalid_network, X}}.

%% @doc Get default port from a given network.
-spec port_number(Network :: network()) -> {ok, Port :: inet:port_number()} | {error, term()}.
port_number(mainnet) ->
    {ok, 8333};
port_number(testnet) ->
    {ok, 18333};
port_number(Network) ->
    {error, {invalid_network, Network}}.

%% @doc Convert a given network atom to a binary.
-spec stringify(Network :: network()) -> {ok, binary()} | {error, term()}.
stringify(mainnet) ->
    {ok, <<"mainnet">>};
stringify(testnet) ->
    {ok, <<"testnet">>};
stringify(testnet3) ->
    {ok, <<"testnet3">>};
stringify(X) ->
    {error, {invalid_network, X}}.

-ifdef(TEST).

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
