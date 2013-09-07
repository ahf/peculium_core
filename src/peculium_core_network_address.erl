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
%%% @doc Bitcoin Network Address Utilities.
%%% This module contains utilities for manipulating and using Network Address
%%% objects.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_network_address).

%% API.
-export([time/1, services/1, address/1, port/1]).

%% Types.
-type uint32_t() :: peculium_core_types:uint32_t().
-type uint16_t() :: peculium_core_types:uint16_t().
-type uint64_t() :: peculium_core_types:uint64_t().
-type network_address() :: peculium_core_types:network_address().

-include("peculium_core.hrl").

%% @doc Returns the timestamp of a given network address.
-spec time(NetworkAddress :: network_address()) -> uint32_t().
time(#network_address { time = Timestamp }) ->
    Timestamp.

%% @doc Returns the services value of a given network address.
-spec services(NetworkAddress :: network_address()) -> uint64_t().
services(#network_address { services = Services }) ->
    Services.

%% @doc Returns the address of a given network address.
-spec address(NetworkAddress :: network_address()) -> inet:ip6_address().
address(#network_address { address = Address }) ->
    Address.

%% @doc Returns the port of a given network address.
-spec port(NetworkAddress :: network_address()) -> uint16_t().
port(#network_address { port = Port }) ->
    Port.
