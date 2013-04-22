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
%%% @doc        Bitcoin Difficulty Utilities.
%%% @reference  <a href="https://en.bitcoin.it/wiki/Difficulty">
%%%                 Bitcoin Difficulty
%%%             </a>
%%%             <em>The Bitcoin community</em>.
%%% ----------------------------------------------------------------------------
-module(peculium_difficulty).

%% API.
-export([from_bits/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Calculates the difficulty from the compact bits representation.
-spec from_bits(uint32_t()) -> number().
from_bits(Bits) ->
    max_difficulty() / difficulty(Bits).

%% @private
-spec difficulty(uint32_t()) -> number().
difficulty(Compact) ->
    %% FIXME: This function could easily be optimized using
    %% shift operators instead of the pow call.
    A = Compact bsr 24,
    B = Compact band 16#007fffff,
    B * math:pow(2, 8 * (A - 3)).

%% @private
-spec max_difficulty() -> number().
max_difficulty() ->
    difficulty(16#1d00ffff).
