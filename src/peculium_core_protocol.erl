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
%%% @doc Bitcoin Protocol Message Decoder.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_protocol).

%% API.
-export([decode/1]).

%% Types.
-type command() :: peculium_core_types:command().
-type message() :: peculium_core_types:message().
-type message_types() :: peculium_core_types:message_types().
-type network() :: peculium_core_types:network().

-include("peculium_core.hrl").

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Try to decode a binary into a message.
-spec decode(Data :: binary()) -> {ok, Message :: message()} | {error, term()}.
decode(Data) ->
    decode_magic_value(Data).

%% @private
-spec decode_magic_value(binary()) -> {ok, message()} | {error, term()}.
decode_magic_value(<<249, 190, 180, 217, Rest/binary>>) ->
    decode_message_frame(mainnet, Rest);
decode_magic_value(<<250, 191, 181, 218, Rest/binary>>) ->
    decode_message_frame(testnet, Rest);
decode_magic_value(<<11, 17, 9, 7, Rest/binary>>) ->
    decode_message_frame(testnet3, Rest);
decode_magic_value(<<Magic:4/binary, Rest/binary>>) ->
    {error, {invalid_magic_value, Magic}, Rest};
decode_magic_value(_X) ->
    {error, insufficient_data}.

%% @private
-spec decode_message_frame(Network :: network(), Data :: binary()) -> {ok, message()} | {error, term()}.
decode_message_frame(Network, <<RawCommand:12/binary, Size:32/little-unsigned-integer, Checksum:4/binary, Rest/binary>> = X) ->
    {ok, Command} = peculium_core_protocol_utilities:command_to_atom(RawCommand),
    case Rest of
        <<Payload:Size/binary, Rest2/binary>> ->
            case decode_message_payload(Command, Payload) of
                {ok, Message} ->
                    {ok, #message {
                        header = #message_header {
                            network = Network,
                            command = Command,
                            length = Size,
                            checksum = Checksum,
                            valid = peculium_core_protocol_utilities:checksum(Payload) =:= Checksum
                        },
                        body = Message
                    }, Rest2};
                {error, Error} ->
                    {error, Error, X};
                _Otherwise ->
                    {error, insufficient_data}
            end;
        _Otherwise ->
            {error, insufficient_data}
    end;

decode_message_frame(_Network, _Data) ->
    {error, insufficient_data}.

-spec decode_message_payload(Command :: command(), Data :: binary()) -> {ok, message_types()} | {error, term()}.
decode_message_payload(verack, <<>>) ->
    {ok, #verack_message {} };

decode_message_payload(ping, <<>>) ->
    {ok, #ping_message {} };

decode_message_payload(getaddr, <<>>) ->
    {ok, #getaddr_message {} };

decode_message_payload(version, <<Version:4/binary, Services:8/binary, Timestamp:8/binary, RawToAddress:26/binary, RawFromAddress:26/binary, Nonce:8/binary, Rest/binary>>) ->
    {ok, FromAddress} = peculium_core_protocol_types:net_addr(RawFromAddress),
    {ok, ToAddress} = peculium_core_protocol_types:net_addr(RawToAddress),
    case peculium_core_protocol_types:var_string(Rest) of
        {ok, UserAgent, <<StartHeight:4/binary>>} ->
            {ok, #version_message {
                version = peculium_core_protocol_types:int32_t(Version),
                services = peculium_core_protocol_types:int64_t(Services),
                timestamp = peculium_core_protocol_types:int64_t(Timestamp),
                user_agent = UserAgent,
                to_address = ToAddress,
                from_address = FromAddress,
                start_height = peculium_core_protocol_types:int32_t(StartHeight),
                relay = true,
                nonce = Nonce
            } };

        {ok, UserAgent, <<StartHeight:4/binary, Relay:1/binary>>} ->
            {ok, #version_message {
                version = peculium_core_protocol_types:int32_t(Version),
                services = peculium_core_protocol_types:int64_t(Services),
                timestamp = peculium_core_protocol_types:int64_t(Timestamp),
                user_agent = UserAgent,
                to_address = ToAddress,
                from_address = FromAddress,
                start_height = peculium_core_protocol_types:int32_t(StartHeight),
                relay = peculium_core_protocol_types:bool(Relay),
                nonce = Nonce
            } };

        {error, _} = Error ->
            Error
    end;

decode_message_payload(alert, X) ->
    case peculium_core_protocol_types:var_string(X) of
        {ok, Payload, Rest} ->
            case peculium_core_protocol_types:var_string(Rest) of
                {ok, Signature, <<>>} ->
                    {ok, #alert_message {
                        payload = Payload,
                        signature = Signature
                    } };
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(inv, X) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 4 + 32,
            VectorSize = Count * ItemSize,
            case Rest of
                <<InvVector:VectorSize/binary>> ->
                    {ok, Inventory, <<>>} = peculium_core_protocol_utilities:decode_vector(InvVector, ItemSize, fun peculium_core_protocol_types:inv/1),
                    {ok, #inv_message {
                        inventory = Inventory
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(getdata, X) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 4 + 32,
            VectorSize = Count * ItemSize,
            case Rest of
                <<InvVector:VectorSize/binary>> ->
                    {ok, Inventory, <<>>} = peculium_core_protocol_utilities:decode_vector(InvVector, ItemSize, fun peculium_core_protocol_types:inv/1),
                    {ok, #getdata_message {
                        inventory = Inventory
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(notfound, X) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 4 + 32,
            VectorSize = Count * ItemSize,
            case Rest of
                <<InvVector:VectorSize/binary>> ->
                    {ok, Inventory, <<>>} = peculium_core_protocol_utilities:decode_vector(InvVector, ItemSize, fun peculium_core_protocol_types:inv/1),
                    {ok, #notfound_message {
                        inventory = Inventory
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(addr, X) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 4 + 8 + 16 + 2,
            VectorSize = Count * ItemSize,
            case Rest of
                <<RawAddresses:VectorSize/binary>> ->
                    {ok, Addresses, <<>>} = peculium_core_protocol_utilities:decode_vector(RawAddresses, ItemSize, fun peculium_core_protocol_types:net_addr/1),
                    {ok, #addr_message {
                        addresses = Addresses
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(headers, X) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 4 + 32 + 32 + 4 + 4 + 4 + 1,
            VectorSize = Count * ItemSize,
            case Rest of
                <<RawHeaders:VectorSize/binary>> ->
                    {ok, BlockHeaders, <<>>} = peculium_core_protocol_utilities:decode_vector(RawHeaders, ItemSize, fun peculium_core_protocol_types:block_header/1),
                    {ok, #headers_message {
                        headers = BlockHeaders
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(getblocks, <<RawVersion:4/binary, X/binary>>) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 32,
            VectorSize = Count * ItemSize,
            case Rest of
                <<Hashes:VectorSize/binary, HashStop:32/binary>> ->
                    {ok, BlockLocatorHashes, <<>>} = peculium_core_protocol_utilities:decode_vector(Hashes, ItemSize, fun(<<Hash:32/binary>>) -> {ok, Hash} end),
                    {ok, #getblocks_message {
                        version = peculium_core_protocol_types:uint32_t(RawVersion),
                        block_locator = BlockLocatorHashes,
                        hash_stop = HashStop
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(getheaders, <<RawVersion:4/binary, X/binary>>) ->
    case peculium_core_protocol_types:var_int(X) of
        {ok, Count, Rest} ->
            ItemSize = 32,
            VectorSize = Count * ItemSize,
            case Rest of
                <<Hashes:VectorSize/binary, HashStop:32/binary>> ->
                    {ok, BlockLocatorHashes, <<>>} = peculium_core_protocol_utilities:decode_vector(Hashes, ItemSize, fun(<<Hash:32/binary>>) -> {ok, Hash} end),
                    {ok, #getheaders_message {
                        version = peculium_core_protocol_types:uint32_t(RawVersion),
                        block_locator = BlockLocatorHashes,
                        hash_stop = HashStop
                    }};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

decode_message_payload(transaction, X) ->
    case peculium_core_protocol_types:transaction(X) of
        {ok, Transaction} ->
            {ok, #transaction_message {
                transaction = Transaction
            }};
        {error, _} = Error ->
            Error
    end;

decode_message_payload(block, X) ->
    case peculium_core_protocol_types:block(X) of
        {ok, Block} ->
            {ok, #block_message {
                block = Block
            }};

        {error, _} = Error ->
            Error
    end;

decode_message_payload(Command, X) ->
    {error, {invalid_command, Command}, X}.
