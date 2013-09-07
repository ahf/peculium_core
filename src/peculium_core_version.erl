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
%%% @doc Bitcoin Version Message Utilities.
%%% This module contains utilities for manipulating and using Bitcoin version
%%% message objects.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_version).

%% API.
-export([version/1, services/1, timestamp/1, to_address/1, from_address/1,
        user_agent/1, start_height/1, nonce/1]).

%% Types.
-type int32_t() :: peculium_core_types:int32_t().
-type int64_t() :: peculium_core_types:int64_t().
-type uint64_t() :: peculium_core_types:uint64_t().
-type version_message() :: peculium_core_types:version_message().
-type network_address() :: peculium_core_types:network_address().

-include("peculium_core.hrl").

%% @doc Returns the version of a given version message.
-spec version(VersionMessage :: version_message()) -> int32_t().
version(#version_message { version = Version }) ->
    Version.

%% @doc Returns the service bitset of a given version message.
-spec services(VersionMessage :: version_message()) -> uint64_t().
services(#version_message { services = Services }) ->
    Services.

%% @doc Returns the timestamp of a given version message.
-spec timestamp(VersionMessage :: version_message()) -> int64_t().
timestamp(#version_message { timestamp = Timestamp }) ->
    Timestamp.

%% @doc Returns the address of the target client of a given version message.
-spec to_address(VersionMessage :: version_message()) -> network_address().
to_address(#version_message { to_address = ToAddress }) ->
    ToAddress.

%% @doc Returns the address of the sending client of a given version message.
-spec from_address(VersionMessage :: version_message()) -> network_address().
from_address(#version_message { from_address = FromAddress }) ->
    FromAddress.

%% @doc Returns the user agent of a given version message.
-spec user_agent(VersionMessage :: version_message()) -> binary().
user_agent(#version_message { user_agent = UserAgent }) ->
    UserAgent.

%% @doc Returns the start height of a given version message.
-spec start_height(VersionMessage :: version_message()) -> int32_t().
start_height(#version_message { start_height = StartHeight }) ->
    StartHeight.

%% @doc Returns the nonce of a given version message.
-spec nonce(VersionMessage :: version_message()) -> binary().
nonce(#version_message { nonce = Nonce }) ->
    Nonce.
