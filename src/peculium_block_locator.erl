%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Locator Utilities.
-module(peculium_block_locator).

%% API.
-export([from_best_block/0, from_height/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Create block locator from the best block.
-spec from_best_block() -> block_locator().
from_best_block() ->
    {ok, Height} = peculium_block_index_srv:best_block_height(),
    from_height(Height).

%% @doc Create block locator object from a given height.
-spec from_height(Height :: non_neg_integer()) -> {ok, block_locator()} | {error, any()}.
from_height(Height) ->
    Indices = from_height(Height, [], 1, 0),
    MapFun = fun (X) ->
                 {ok, Hash} = peculium_block_index_srv:height_to_hash(X),
                 peculium_utilities:reverse(Hash)
              end,
    lists:map(MapFun, Indices).

%% @private
-spec from_height(Count :: non_neg_integer(), Result :: list(non_neg_integer()), Step :: non_neg_integer(), Count :: non_neg_integer()) -> [non_neg_integer()].
from_height(Count, Result, Step, Start) when Start >= 10, Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step * 2, Start + 1);
from_height(Count, Result, Step, Start) when Count >= 1 ->
    from_height(Count - Step, [Count | Result], Step, Start + 1);
from_height(_Count, Result, _Step, _Start) ->
    lists:reverse([0 | Result]).
