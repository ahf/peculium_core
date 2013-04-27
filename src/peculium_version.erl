%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions.
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
%%% @doc        Bitcoin Version Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_version).

%% API.
-export([version/1, services/1, timestamp/1, to_address/1, from_address/1, user_agent/1, start_height/1, nonce/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the version of a given version message.
-spec version(bitcoin_version_message()) -> int32_t().
version(#bitcoin_version_message { version = Version }) ->
    Version.

%% @doc Returns the service bitset of a given version message.
-spec services(bitcoin_version_message()) -> uint64_t().
services(#bitcoin_version_message { services = Services }) ->
    Services.

%% @doc Returns the timestamp of a given version message.
-spec timestamp(bitcoin_version_message()) -> int64_t().
timestamp(#bitcoin_version_message { timestamp = Timestamp }) ->
    Timestamp.

%% @doc Returns the address of the target client of a given version message.
-spec to_address(bitcoin_version_message()) -> bitcoin_network_address().
to_address(#bitcoin_version_message { to_address = ToAddress }) ->
    ToAddress.

%% @doc Returns the address of the sending client of a given version message.
-spec from_address(bitcoin_version_message()) -> bitcoin_network_address().
from_address(#bitcoin_version_message { from_address = FromAddress }) ->
    FromAddress.

%% @doc Returns the user agent of a given version message.
-spec user_agent(bitcoin_version_message()) -> binary().
user_agent(#bitcoin_version_message { user_agent = UserAgent }) ->
    UserAgent.

%% @doc Returns the start height of a given version message.
-spec start_height(bitcoin_version_message()) -> int32_t().
start_height(#bitcoin_version_message { start_height = StartHeight }) ->
    StartHeight.

%% @doc Returns the nonce of a given version message.
-spec nonce(bitcoin_version_message()) -> binary().
nonce(#bitcoin_version_message { nonce = Nonce }) ->
    Nonce.
