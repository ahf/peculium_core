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
%%% @doc JSON Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_json).

%% API.
-export([encode/1]).

-include_lib("peculium_core/include/peculium_core.hrl").

%% @doc Serialize Bitcoin message to JSON.
encode(#message { header = Header, body = Body }) ->
    [
        {<<"header">>, encode(Header)},
        {<<"body">>, encode(Body)}
    ];

encode(#message_header { network = Network, command = Command, length = Length, checksum = Checksum, valid = Valid }) ->
    [
        {<<"network">>, atom_to_binary(Network, utf8)},
        {<<"command">>, atom_to_binary(Command, utf8)},
        {<<"length">>, Length},
        {<<"checksum">>, peculium_core_utilities:bin2hex(Checksum)},
        {<<"valid">>, Valid}
    ];

encode(#verack_message {}) ->
    null;

encode(#ping_message {}) ->
    null;

encode(#getaddr_message {}) ->
    null;

encode(#version_message { version = Version, services = Services, timestamp = Timestamp, to_address = ToAddress, from_address = FromAddress, user_agent = UserAgent, start_height = StartHeight, relay = Relay, nonce = Nonce }) ->
    [
        {<<"version">>, Version},
        {<<"services">>, Services},
        {<<"timestamp">>, Timestamp},
        {<<"to_address">>, encode(ToAddress)},
        {<<"from_address">>, encode(FromAddress)},
        {<<"user_agent">>, UserAgent},
        {<<"start_height">>, StartHeight},
        {<<"relay">>, Relay},
        {<<"nonce">>, peculium_core_utilities:bin2hex(Nonce)}
    ];

encode(#alert_message { payload = Payload, signature = Signature }) ->
    [
        {<<"payload">>, peculium_core_utilities:bin2hex(Payload)},
        {<<"signature">>, peculium_core_utilities:bin2hex(Signature)}
    ];

encode(#inv_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#getdata_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#notfound_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#addr_message { addresses = Addresses }) ->
    [
        {<<"addresses">>, lists:map(fun encode/1, Addresses)}
    ];

encode(#headers_message { headers = Headers }) ->
    [
        {<<"headers">>, lists:map(fun encode/1, Headers)}
    ];

encode(#getblocks_message { version = Version, block_locator = BlockLocatorHashes, hash_stop = HashStop }) ->
    [
        {<<"version">>, Version},
        {<<"block_locator">>, lists:map(fun peculium_core_utilities:bin2hex/1, BlockLocatorHashes)},
        {<<"hash_stop">>, peculium_core_utilities:bin2hex(HashStop)}
    ];

encode(#getheaders_message { version = Version, block_locator = BlockLocatorHashes, hash_stop = HashStop }) ->
    [
        {<<"version">>, Version},
        {<<"block_locator">>, lists:map(fun peculium_core_utilities:bin2hex/1, BlockLocatorHashes)},
        {<<"hash_stop">>, peculium_core_utilities:bin2hex(HashStop)}
    ];

encode(#transaction { version = Version, transaction_inputs = Inputs, transaction_outputs = Outputs, lock_time = LockTime }) ->
    [
        {<<"version">>, Version},
        {<<"transaction_inputs">>, lists:map(fun encode/1, Inputs)},
        {<<"transaction_outputs">>, lists:map(fun encode/1, Outputs)},
        {<<"lock_time">>, LockTime}
    ];

encode(#block { version = Version, previous_block = PreviousBlock, merkle_root = MerkleRoot, timestamp = Timestamp, bits = Bits, nonce = Nonce, transactions = Transactions }) ->
    [
        {<<"version">>, Version},
        {<<"previous_block">>, peculium_core_utilities:bin2hex(PreviousBlock)},
        {<<"merkle_root">>, peculium_core_utilities:bin2hex(MerkleRoot)},
        {<<"timestamp">>, Timestamp},
        {<<"bits">>, Bits},
        {<<"nonce">>, Nonce},
        {<<"transactions">>, lists:map(fun encode/1, Transactions)}
    ];

encode(#transaction_outpoint { index = Index, hash = Hash }) ->
    [
        {<<"index">>, Index},
        {<<"hash">>, peculium_core_utilities:bin2hex(Hash)}
    ];

encode(#transaction_input { previous_output = PreviousOutput, script = Script, sequence = Sequence }) ->
    [
        {<<"previous_output">>, encode(PreviousOutput)},
        {<<"script">>, peculium_core_utilities:bin2hex(Script)},
        {<<"sequence">>, Sequence}
    ];

encode(#transaction_output { value = Value, script = Script }) ->
    [
        {<<"value">>, Value},
        {<<"script">>, peculium_core_utilities:bin2hex(Script)}
    ];

encode(#inv { type = Type, hash = Hash }) ->
    [
        {<<"type">>, atom_to_binary(Type, utf8)},
        {<<"hash">>, peculium_core_utilities:bin2hex(Hash)}
    ];

encode(#network_address { time = Time, services = Services, address = Address, port = Port }) ->
    [
        {<<"time">>, Time},
        {<<"services">>, Services},
        {<<"address">>, list_to_binary(inet_parse:ntoa(Address))},
        {<<"port">>, Port}
    ].