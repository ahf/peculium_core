%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions. All rights reserved.
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
%%% @doc        Bitcoin Network Address Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_net_address).

%% API.
-export([time/1, services/1, address/1, port/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the timestamp of a given network address.
-spec time(NetworkAddress :: bitcoin_net_address()) -> uint32_t().
time(#bitcoin_net_address { time = Timestamp }) ->
    Timestamp.

%% @doc Returns the services value of a given network address.
-spec services(NetworkAddress :: bitcoin_net_address()) -> uint64_t().
services(#bitcoin_net_address { services = Services }) ->
    Services.

%% @doc Returns the address of a given network address.
-spec address(NetworkAddress :: bitcoin_net_address()) -> inet:ip6_address().
address(#bitcoin_net_address { address = Address }) ->
    Address.

%% @doc Returns the port of a given network address.
-spec port(NetworkAddress :: bitcoin_net_address()) -> uint16_t().
port(#bitcoin_net_address { port = Port }) ->
    Port.
