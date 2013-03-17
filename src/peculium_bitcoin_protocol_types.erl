%% Copyright (c) 2013 Alexander Færøy
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(peculium_bitcoin_protocol_types).

-export([int8_t/1, int16_t/1, int32_t/1, int64_t/1]).
-export([uint8_t/1, uint16_t/1, uint32_t/1, uint64_t/1]).
-export([var_int/1, var_string/1]).
-export([net_addr/1, map_to_v6/1]).
-export([bool/1]).
-export([inv/1, block_header/1, outpoint/1]).
-export([transaction_input/1, transaction_outpoint/1]).

-include_lib("peculium/include/peculium.hrl").
-include_lib("kernel/include/inet.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec int8_t(int8_t()) -> integer();
            (binary()) -> int8_t().
int8_t(X) when is_integer(X) ->
    <<X:8/little-signed-integer>>;
int8_t(<<X:8/little-signed-integer>>) ->
    X.

-spec int16_t(int16_t()) -> integer();
             (binary()) -> int16_t().
int16_t(X) when is_integer(X) ->
    <<X:16/little-signed-integer>>;
int16_t(<<X:16/little-signed-integer>>) ->
    X.

-spec int32_t(int32_t()) -> integer();
             (binary()) -> int32_t().
int32_t(X) when is_integer(X) ->
    <<X:32/little-signed-integer>>;
int32_t(<<X:32/little-signed-integer>>) ->
    X.

-spec int64_t(int64_t()) -> integer();
             (binary()) -> int64_t().
int64_t(X) when is_integer(X) ->
    <<X:64/little-signed-integer>>;
int64_t(<<X:64/little-signed-integer>>) ->
    X.

-spec uint8_t(uint8_t()) -> non_neg_integer();
             (binary()) -> uint8_t().
uint8_t(X) when is_integer(X) ->
    <<X:8/little-unsigned-integer>>;
uint8_t(<<X:8/little-unsigned-integer>>) ->
    X.

-spec uint16_t(uint16_t()) -> non_neg_integer();
              (binary()) -> uint16_t().
uint16_t(X) when is_integer(X) ->
    <<X:16/little-unsigned-integer>>;
uint16_t(<<X:16/little-unsigned-integer>>) ->
    X.

-spec uint32_t(uint32_t()) -> non_neg_integer();
              (binary()) -> uint32_t().
uint32_t(X) when is_integer(X) ->
    <<X:32/little-unsigned-integer>>;
uint32_t(<<X:32/little-unsigned-integer>>) ->
    X.

-spec uint64_t(uint64_t()) -> non_neg_integer();
              (binary()) -> uint64_t().
uint64_t(X) when is_integer(X) ->
    <<X:64/little-unsigned-integer>>;
uint64_t(<<X:64/little-unsigned-integer>>) ->
    X.

-spec var_int(binary()) -> {ok, integer(), binary()} | {error, {invalid_var_int, any()}};
             (integer()) -> {ok, iolist()} | {error, any()}.
var_int(<<X:8/little-unsigned-integer, Rest/binary>>) when X < 16#fd ->
    {ok, X, Rest};
var_int(<<16#fd:8, X:16/little-unsigned-integer, Rest/binary>>) ->
    {ok, X, Rest};
var_int(<<16#fe:8, X:32/little-unsigned-integer, Rest/binary>>) ->
    {ok, X, Rest};
var_int(<<16#ff:8, X:64/little-unsigned-integer, Rest/binary>>) ->
    {ok, X, Rest};
var_int(X) when is_integer(X), X < 16#fd ->
    {ok, [uint8_t(X)]};
var_int(X) when is_integer(X), X =< 16#fff ->
    {ok, [uint8_t(16#fd), uint16_t(X)]};
var_int(X) when is_integer(X), X =< 16#ffffffff ->
    {ok, [uint8_t(16#fe), uint32_t(X)]};
var_int(X) when is_integer(X) ->
    {ok, [uint8_t(16#ff), uint64_t(X)]};
var_int(X) ->
    {error, {invalid_var_int, X}}.

-spec var_string(binary()) -> {ok, binary(), binary()} | {error, any()}.
var_string(X) when is_binary(X) ->
    case var_int(X) of
        {ok, Length, Rest} ->
            case Rest of
                <<Result:Length/binary, Rest2/binary>> ->
                    {ok, Result, Rest2};
                _Otherwise ->
                    {error, {invalid_var_int, insufficient_data}}
            end;
        Error ->
            Error
    end.

-spec map_to_v6(inet:ip_address()) -> {ok, inet:ip6_address()} | {error, any()}.
map_to_v6({A, B, C, D}) ->
    {ok, {0, 0, 0, 0, 16#ffff, 16#ffff, (A bsl 8) + B, (C bsl 8) + D}};
map_to_v6(Address) when is_tuple(Address), tuple_size(Address) == 8 ->
    {ok, Address};
map_to_v6(X) ->
    {error, {invalid_address, X}}.

-spec unpack_address(binary()) -> [integer()].
unpack_address(<<>>) ->
    [];
unpack_address(<<X:16/big, Rest/binary>>) ->
    [X | unpack_address(Rest)].

-spec net_addr(binary()) -> {ok, bitcoin_net_address()}.
net_addr(<<Time:4/binary, Services:8/binary, Address:16/binary, Port:2/binary>>) ->
    {ok, #bitcoin_net_address {
        time = uint32_t(Time),
        services = uint64_t(Services),
        address = list_to_tuple(unpack_address(Address)),
        port = uint16_t(Port)
    } };
net_addr(<<X:26/binary>>) ->
    net_addr(<<0, 0, 0, 0, X/binary>>).

-spec bool(uint8_t()) -> boolean().
bool(X) ->
    uint8_t(X) =/= 0.

-spec inv(binary()) -> {ok, bitcoin_inv()}.
inv(<<Type:4/binary, Hash:32/binary>>) ->
    {ok, Type} = peculium_bitcoin_protocol_utilities:inv_to_atom(uint32_t(Type)),
    {ok, #bitcoin_inv {
        type = Type,
        hash = Hash
    } }.

-spec block_header(binary()) -> {ok, bitcoin_block_header()}.
block_header(<<RawVersion:4/binary, PreviousBlock:32/binary, MerkleRoot:32/binary, RawTimestamp:4/binary, RawBits:4/binary, RawNonce:4/binary, RawTransactionCount:1/binary>>) ->
    {ok, #bitcoin_block_header {
        version = uint32_t(RawVersion),
        previous_block = PreviousBlock,
        merkle_root = MerkleRoot,
        timestamp = uint32_t(RawTimestamp),
        bits = uint32_t(RawBits),
        nonce = uint32_t(RawNonce),
        transaction_count = uint8_t(RawTransactionCount)
    } }.

-spec outpoint(binary()) -> {ok, bitcoin_outpoint()}.
outpoint(<<Hash:32/binary, Index:4/binary>>) ->
    {ok, #bitcoin_outpoint {
        index = Index,
        hash = Hash
    } }.

-spec transaction_input(binary()) -> {ok, bitcoin_transaction_input()}.
transaction_input(<<RawOutpoint:36/binary, X/binary>>) ->
    {ok, Outpoint} = outpoint(RawOutpoint),
    case var_int(X) of
        {ok, Length, Rest} ->
            case Rest of
                <<Script:Length/binary, Sequence:4/binary, Rest1/binary>> ->
                    {ok, #bitcoin_transaction_input {
                        previous_output = Outpoint,
                        script = Script,
                        sequence = uint32_t(Sequence)
                    }, Rest1};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec transaction_outpoint(binary()) -> {ok, bitcoin_transaction_output()}.
transaction_outpoint(<<Value:8/binary, X/binary>>) ->
    case var_int(X) of
        {ok, Length, Rest} ->
            case Rest of
                <<Script:Length/binary, Rest1/binary>> ->
                    {ok, #bitcoin_transaction_output {
                        value = int64_t(Value),
                        script = Script
                    }, Rest1};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-ifdef(TEST).

-spec test() -> any().

-endif.
