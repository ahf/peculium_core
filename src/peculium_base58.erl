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
%%% @doc Base58 Encoding and Decoding Utilities.
%%% Base58 is used in the Bitcoin stack to encode public and private keys in
%%% human-typable strings. For instance, a Bitcoin address is a hash of the
%%% public key with a checksum appended and then converted to Base58 encoding.
%%%
%%% The original Satoshi client source code discusses the reasoning behind the
%%% Base58 encoding as the following:
%%%
%%%   - Avoid 0, O, I and l characters as they look the same in some fonts and
%%%   could be used to trick people into transferring Bitcoins to the wrong
%%%   address.
%%%
%%%   - A string with non-alphanumeric characters is not easily accepted as an
%%%   account number.
%%%
%%%   - An email usually won't add a line-break unless there's punctuation to
%%%   break it.
%%%
%%%   - Double clicking selects the whole word and not just a section of the
%%%   word.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_base58).

%% API.
-export([encode/1, decode/1]).

-include("peculium_test.hrl").

-define(BASE58_TABLE, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).

%% @doc Encode data using Base58.
-spec encode(Data :: binary()) -> binary().
encode(<<>>) ->
    <<>>;
encode(<<0, Rest/binary>>) ->
    Result = encode(Rest),
    <<$1, Result/binary>>;
encode(X) ->
    Value = binary:decode_unsigned(X),
    encode(Value, <<>>).

%% @private
-spec encode(non_neg_integer(), binary()) -> binary().
encode(0, Data) ->
    Data;
encode(N, Data) ->
    Symbol = symbol(N rem 58),
    encode(N div 58, <<Symbol:8/unsigned, Data/binary>>).

%% @doc Decode Base58 data.
-spec decode(Data :: binary()) -> {ok, binary()} | {error, {invalid_byte, binary()}}.
decode(<<>>) ->
    {ok, <<>>};
decode(<<$1, Rest/binary>>) ->
    case decode(Rest) of
        {ok, Data} ->
            {ok, <<0, Data/binary>>};
        {error, _} = Error ->
            Error
    end;
decode(X) ->
    case decode(X, 0) of
        {ok, Data} ->
            {ok, binary:encode_unsigned(Data)};
        {error, _} = Error ->
            Error
    end.

%% @private
-spec decode(binary(), non_neg_integer()) -> {ok, binary()} | {error, {invalid_byte, binary()}}.
decode(<<>>, N) ->
    {ok, N};
decode(<<Symbol:8/unsigned, Rest/binary>>, N) ->
    case position(Symbol) of
        {ok, Position} ->
            decode(Rest, N * 58 + Position);
        {error, _} = Error ->
            Error
    end.

%% @private
-spec symbol(non_neg_integer()) -> byte().
symbol(Position) ->
    binary:at(?BASE58_TABLE, Position).

%% @private
-spec position(byte()) -> {ok, non_neg_integer()} | {error, {invalid_byte, binary()}}.
position(Symbol) ->
    position(Symbol, 0, ?BASE58_TABLE).

%% @private.
-spec position(byte(), non_neg_integer(), binary()) -> {ok, non_neg_integer()} | {error, {invalid_byte, binary()}}.
position(Symbol, N, <<X:8/unsigned, Rest/binary>>) ->
    case X of
        Symbol ->
            {ok, N};
        _Otherwise ->
            position(Symbol, N + 1, Rest)
    end;
position(Symbol, _, <<>>) ->
    {error, {invalid_byte, <<Symbol>>}}.

-ifdef(TEST).

-spec prop_inverse() -> any().
prop_inverse() ->
    ?FORALL(X, binary(),
        decode(encode(X)) =:= {ok, X}).

-spec prop_inverse2() -> any().
prop_inverse2() ->
    ?FORALL(X, binary(),
        decode(encode(<<0, X/binary>>)) =:= {ok, <<0, X/binary>>}).

-endif.
