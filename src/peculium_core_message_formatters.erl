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
%%% @doc Bitcoin message formatters
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_message_formatters).

%% API.
-export([format/1]).

%% Types.
-type message() :: peculium_core_types:message().
-type message_types() :: peculium_core_types:message_types().

-include("peculium_core.hrl").

-spec format(Message :: message()) -> {ok, string()} | {error, term()};
            (Message :: message_types()) -> {ok, string()} | {error, term()}.
format(#message { body = Body }) ->
    format(Body);
format(#verack_message {}) ->
    {ok, "verack"};
format(#ping_message {}) ->
    {ok, "ping"};
format(#getaddr_message {}) ->
    {ok, "getaddr"};
format(#version_message { version = Version, services = Services, timestamp = Timestamp, user_agent = UserAgent, start_height = StartHeight, relay = Relay }) ->
    {ok, io_lib:format("version (version=~b, services=~b, timestamp=~b, user_agent=\"~s\", start_height=~b, relay=~p)", [Version, Services, Timestamp, binary_to_list(UserAgent), StartHeight, Relay])};
format(#alert_message {}) ->
    {ok, "alert"};
format(#inv_message { inventory = Invs }) ->
    {ok, io_lib:format("inv (length=~b)", [length(Invs)])};
format(#getdata_message { inventory = Invs }) ->
    {ok, io_lib:format("getdata (length=~b)", [length(Invs)])};
format(#notfound_message { inventory = Invs }) ->
    {ok, io_lib:format("notfound (length=~b)", [length(Invs)])};
format(#addr_message { addresses = Addresses }) ->
    {ok, io_lib:format("addr (length=~b)", [length(Addresses)])};
format(#headers_message { headers = Headers }) ->
    {ok, io_lib:format("headers (length=~b)", [length(Headers)])};
format(#getblocks_message { version = Version, block_locator = BlockLocator, hash_stop = HashStop }) ->
    {ok, io_lib:format("getblocks (version=~b, block_locator=~b, hash_stop=~s)", [Version, length(BlockLocator), binary_to_list(peculium_core_utilities:bin2hex(HashStop))])};
format(#getheaders_message { version = Version, block_locator = BlockLocator, hash_stop = HashStop }) ->
    {ok, io_lib:format("getheaders (version=~b, block_locator=~b, hash_stop=~s)", [Version, length(BlockLocator), binary_to_list(peculium_core_utilities:bin2hex(HashStop))])};
format(#transaction_message { transaction = Transaction }) ->
    {ok, io_lib:format("tx (hash=~s)", [binary_to_list(peculium_core_utilities:bin2hex(peculium_core_transaction:hash(Transaction)))])};
format(#block_message { block = Block }) ->
    {ok, io_lib:format("block (hash=~s)", [binary_to_list(peculium_core_utilities:bin2hex(peculium_core_block:hash(Block)))])};
format(X) ->
    {error, {unknown_message, X}}.
