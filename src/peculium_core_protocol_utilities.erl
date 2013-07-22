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
%%% @doc Bitcoin Protocol Utilities.
%%% This module contains utilities used in the Bitcoin protocol encoder and
%%% decoder.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_protocol_utilities).

%% API.
-export([inv_to_atom/1, atom_to_inv/1 , command_to_atom/1 , checksum/1,
        decode_vector/3, decode_dynamic_vector/3]).

%% Types.
-type checksum() :: peculium_core_types:checksum().
-type inv_type() :: peculium_core_types:inv_type().
-type inv_integer() :: peculium_core_types:inv_integer().
-type command() :: peculium_core_types:command().

-type vector_decode_fun() :: fun((Data :: binary()) -> {ok, Item :: any()} | {error, any()}).
-type dynamic_vector_decode_fun() :: fun((Data :: binary()) -> {ok, Item :: any(), Rest :: binary()} | {error, any()}).

%% @doc Returns the first four bytes of the double SHA256 checksum of the given Data.
-spec checksum(Data :: iolist()) -> checksum().
checksum(Data) ->
    binary_part(crypto:sha256(crypto:sha256(Data)), {0, 4}).

%% @doc Returns an inv atom from a given integer.
-spec inv_to_atom(InvInteger :: integer()) -> {ok, inv_type()} | {error, any()}.
inv_to_atom(0) ->
    {ok, error};
inv_to_atom(1) ->
    {ok, transaction};
inv_to_atom(2) ->
    {ok, block};
inv_to_atom(X) ->
    {error, {invalid_inv_integer, X}}.

%% @doc Returns an integer from a given inv type.
-spec atom_to_inv(Inv :: inv_type()) -> {ok, inv_integer()} | {error, any()}.
atom_to_inv(error) ->
    {ok, 0};
atom_to_inv(transaction) ->
    {ok, 1};
atom_to_inv(block) ->
    {ok, 2};
atom_to_inv(X) ->
    {error, {invalid_inv_type, X}}.

%% @doc Returns a command atom from a given binary.
-spec command_to_atom(Command :: binary()) -> {ok, command()} | {error, any()}.
command_to_atom(Command) ->
    case peculium_core_utilities:strip(Command, <<0>>) of
        <<"addr">> ->
            {ok, addr};
        <<"alert">> ->
            {ok, alert};
        <<"block">> ->
            {ok, block};
        <<"checkorder">> ->
            {ok, checkorder};
        <<"getaddr">> ->
            {ok, getaddr};
        <<"notfound">> ->
            {ok, notfound};
        <<"getblocks">> ->
            {ok, getblocks};
        <<"getdata">> ->
            {ok, getdata};
        <<"getheaders">> ->
            {ok, getheaders};
        <<"headers">> ->
            {ok, headers};
        <<"inv">> ->
            {ok, inv};
        <<"ping">> ->
            {ok, ping};
        <<"reply">> ->
            {ok, reply};
        <<"submitorder">> ->
            {ok, submitorder};
        <<"tx">> ->
            %% NOTE: Renaming to 'transaction'.
            {ok, transaction};
        <<"verack">> ->
            {ok, verack};
        <<"version">> ->
            {ok, version};
        Value ->
            {error, {invalid_command, Value}}
    end.

%% @doc Decode a vector where the size of each item is known.
-spec decode_vector(Data :: binary(), ItemSize :: non_neg_integer(), ItemDecodeFun :: vector_decode_fun()) -> {ok, [any()], binary()} | {error, any()}.
decode_vector(Data, ItemSize, ItemDecodeFun) ->
    try
        decode_one_vector(Data, ItemSize, ItemDecodeFun)
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end.

%% @private
-spec decode_one_vector(Data :: binary(), ItemSize :: non_neg_integer(), ItemDecodeFun :: vector_decode_fun()) -> {ok, [any()], binary()} | {error, any()}.
decode_one_vector(Data, ItemSize, ItemDecodeFun) ->
    case Data of
        <<Item:ItemSize/binary, Rest/binary>> ->
            case ItemDecodeFun(Item) of
                {ok, DecodedItem} ->
                    {ok, Tail, Rest2} = decode_one_vector(Rest, ItemSize, ItemDecodeFun),
                    {ok, [DecodedItem | Tail], Rest2};
                {error, Reason} ->
                    throw({error, Reason})
            end;
        _Otherwise ->
            {ok, [], Data}
    end.

%% @doc Decode a vector where the size of each item is unknown.
%% The Bitcoin protocol uses vectors where each element size is unknown until
%% the time of item decoding.
%%
%% This function takes a decode function that consumes the bytes needed to
%% decode an item and returns the decoded item together with the rest of the
%% bytes. This is applied recursively to the remaining bytes until we have
%% decoded `ItemCount' number of items or if an error occurs.
%% @end
-spec decode_dynamic_vector(Data :: binary(), ItemCount :: non_neg_integer(), ItemDecodeFun :: dynamic_vector_decode_fun()) -> {ok, [any()], binary()} | {error, any()}.
decode_dynamic_vector(Data, ItemCount, ItemDecodeFun) ->
    try
        decode_one_dynamic_vector(Data, ItemCount, ItemDecodeFun)
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end.

%% @private
-spec decode_one_dynamic_vector(Data :: binary(), ItemCount :: non_neg_integer(), ItemDecodeFun :: dynamic_vector_decode_fun()) -> {ok, [any()], binary()} | {error, any()}.
decode_one_dynamic_vector(Data, 0, _ItemDecodeFun) ->
    {ok, [], Data};
decode_one_dynamic_vector(Data, ItemCount, ItemDecodeFun) ->
    case ItemDecodeFun(Data) of
        {ok, Item, Rest} ->
            {ok, Tail, Rest2} = decode_one_dynamic_vector(Rest, ItemCount - 1, ItemDecodeFun),
            {ok, [Item | Tail], Rest2};
        {error, Reason} ->
            throw({error, Reason})
    end.
