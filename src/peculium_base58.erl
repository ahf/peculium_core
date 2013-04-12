%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Base58 encoding and decoding utilities.
%% @reference <a href="https://en.bitcoin.it/wiki/Base58Check_encoding">Base58Check encoding</a>
%%            <em>The Bitcoin community</em>.
-module(peculium_base58).

%% API.
-export([encode/1, decode/1]).

-include("peculium_test.hrl").

-define(BASE58_TABLE, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).

%% @doc Encode a binary using Base58.
-spec encode(binary()) -> binary().
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

%% @doc Decode a Base58 binary.
-spec decode(binary()) -> {ok, binary()} | {error, {invalid_byte, binary()}}.
decode(<<>>) ->
    {ok, <<>>};
decode(<<$1, Rest/binary>>) ->
    case decode(Rest) of
        {ok, Data} ->
            {ok, <<0, Data/binary>>};
        Error ->
            Error
    end;
decode(X) ->
    case decode(X, 0) of
        {ok, Data} ->
            {ok, binary:encode_unsigned(Data)};
        Error ->
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
        Error ->
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
        decode(encode(X)) == {ok, X}).

-endif.
