%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Locator Utilities.
-module(peculium_block_locator).

%% API.
-export([from_height/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Create block locator object from a given height.
-spec from_height(Height :: non_neg_integer()) -> {ok, block_locator()} | {error, any()}.
from_height(Height) ->
    from_height(Height, [], 1, 0).

%% @private
-spec from_height(Count :: non_neg_integer(), Result :: list(non_neg_integer()), Step :: non_neg_integer(), Count :: non_neg_integer()) -> [non_neg_integer()].
from_height(Count, Result, Step, Start) when Start >= 10, Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step * 2, Start + 1);
from_height(Count, Result, Step, Start) when Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step, Start + 1);
from_height(_Count, Result, _Step, _Start) ->
    lists:reverse([0 | Result]).
