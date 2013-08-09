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
%%% @doc        Bitcoin Block Locator Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_core_block_locator).

%% API.
-export([from_best_block/0, from_height/1]).

%% Types.
-type block_locator() :: peculium_core_types:block_locator().

%% @doc Create block locator from the best block.
-spec from_best_block() -> block_locator().
from_best_block() ->
    {ok, Height} = peculium_core_block_index:best_block_height(),
    from_height(Height).

%% @doc Create block locator object from a given height.
-spec from_height(Height :: non_neg_integer()) -> {ok, block_locator()} | {error, any()}.
from_height(Height) ->
    Indices = from_height(Height, [], 1, 0),
    MapFun = fun (X) ->
                 {ok, Hash} = peculium_core_block_index:height_to_hash(X),
                 peculium_core_utilities:reverse(Hash)
              end,
    lists:map(MapFun, Indices).

%% @private
-spec from_height(Count :: non_neg_integer(), Result :: [non_neg_integer()], Step :: non_neg_integer(), Count :: non_neg_integer()) -> [non_neg_integer()].
from_height(Count, Result, Step, Start) when Start >= 10, Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step * 2, Start + 1);
from_height(Count, Result, Step, Start) when Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step, Start + 1);
from_height(_Count, Result, _Step, _Start) ->
    lists:reverse([0 | Result]).
