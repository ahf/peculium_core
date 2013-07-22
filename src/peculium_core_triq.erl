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
%%% @doc Triq Type Domains.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_triq).

%% API.
-export([uint8_t/0, uint16_t/0, uint32_t/0, uint64_t/0, int8_t/0, int16_t/0,
        int32_t/0, int64_t/0, unit_type/0, binary4/0, binary8/0,
        binary16/0, binary32/0]).

-include_lib("triq/include/triq.hrl").

%% @doc The domain of the uint8_t type.
-spec uint8_t() -> triq_dom:domains(any()).
uint8_t() ->
    choose(0, 16#ff).

%% @doc The domain of the uint16_t type.
-spec uint16_t() -> triq_dom:domains(any()).
uint16_t() ->
    choose(0, 16#ffff).

%% @doc The domain of the uint32_t type.
-spec uint32_t() -> triq_dom:domains(any()).
uint32_t() ->
    choose(0, 16#ffffffff).

%% @doc The domain of the uint64_t type.
-spec uint64_t() -> triq_dom:domains(any()).
uint64_t() ->
    choose(0, 16#ffffffffffffffff).

%% @doc The domain of the int8_t type.
-spec int8_t() -> triq_dom:domains(any()).
int8_t() ->
    choose(-16#80, 16#7f).

%% @doc The domain of the int16_t type.
-spec int16_t() -> triq_dom:domains(any()).
int16_t() ->
    choose(-16#8000, 16#7fff).

%% @doc The domain of the uint32_t type.
-spec int32_t() -> triq_dom:domains(any()).
int32_t() ->
    choose(-16#80000000, 16#7fffffff).

%% @doc The domain of the uint64_t type.
-spec int64_t() -> triq_dom:domains(any()).
int64_t() ->
    choose(-16#8000000000000000, 16#7fffffffffffffff).

%% @doc The domain of the bitcoin unit type.
-spec unit_type() -> triq_dom:domain(any()).
unit_type() ->
    elements([satoshi, microbitcoin, millibitcoin, centibitcoin, decibitcoin, bitcoin, decabitcoin, hectobitcoin, kilobitcoin, megabitcoin]).

%% @doc The domain of a fixed length 4 byte binary.
-spec binary4() -> triq_dom:domains(any()).
binary4() ->
    binary(4).

%% @doc The domain of a fixed length 8 byte binary.
-spec binary8() -> triq_dom:domains(any()).
binary8() ->
    binary(8).

%% @doc The domain of a fixed length 16 byte binary.
-spec binary16() -> triq_dom:domains(any()).
binary16() ->
    binary(16).

%% @doc The domain of a fixed length 32 byte binary.
-spec binary32() -> triq_dom:domains(any()).
binary32() ->
    binary(32).
