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
%%% @doc Merkle Tree Utilities.
%%% This module contains utilities for creating and traversing Merkle Tree's.
%%% objects.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_merkle_tree).

%% API.
-export([from_transactions/1, hash/1, left/1, right/1]).

%% Types.
-export_type([merkle_tree_node/0]).

-type transaction() :: peculium_core_types:transaction().
-type hash() :: peculium_core_types:hash().
-type hash_function() :: peculium_core_types:hash_function().

-record(merkle_tree_node, {
    hash :: hash(),
    left :: merkle_tree_node(),
    right :: merkle_tree_node()
}).

-opaque merkle_tree_node() :: #merkle_tree_node {} | hash() | undefined.

-include_lib("peculium_core/include/peculium_core.hrl").

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Create a Merkle tree from a list of transactions.
%% This function takes a list of transactions and returns the root node of the
%% Merkle tree.
%% @end
-spec from_transactions(Transactions :: [transaction(), ...]) -> merkle_tree_node().
from_transactions(Transactions) ->
    from_list(lists:map(fun peculium_core_transaction:hash/1, Transactions), fun peculium_core_crypto:hash/1).

%% @doc Utility function for getting the hash of a given Merkle tree node.
-spec hash(TreeNode :: merkle_tree_node()) -> hash().
hash(#merkle_tree_node { hash = Hash }) ->
    Hash.

%% @doc Utility function for getting the left child of a given Merkle tree node.
-spec left(TreeNode :: merkle_tree_node()) -> merkle_tree_node().
left(#merkle_tree_node { left = LeftChild }) ->
    LeftChild.

%% @doc Utility function for getting the right child of a given Merkle tree node.
-spec right(TreeNode :: merkle_tree_node()) -> merkle_tree_node().
right(#merkle_tree_node { right = RightChild }) ->
    RightChild.

%% @private
-spec from_list(Binaries :: [hash(), ...], hash_function()) -> merkle_tree_node().
from_list(Hashes, Hash) when is_list(Hashes) ->
    from_list(Hashes, [], Hash).

%% @private
-spec from_list(Binaries :: [hash(), ...], TreeNodes :: [merkle_tree_node()], HashFun :: hash_function()) -> merkle_tree_node().
from_list([], Result, Hash) ->
    case Result of
        [Node] ->
            Node;
        [] ->
            erlang:error(badarg);
        Nodes ->
            from_list(lists:reverse(Nodes), [], Hash)
    end;
from_list([A], [], Hash) when is_binary(A) ->
    %% FIXME: Odd case. If there's only one transaction, we must return the hash of that transaction.
    from_list([], [merkle_tree_node(A)], Hash);
from_list([A], Result, Hash) when is_binary(A) ->
    from_list([], [merkle_tree_node(A, A, Hash) | Result], Hash);
from_list([A], Result, Hash) when is_record(A, merkle_tree_node) ->
    from_list([A, A], Result, Hash);
from_list([A, B | Rest], Result, Hash) ->
    from_list(Rest, [merkle_tree_node(A, B, Hash) | Result], Hash).

%% @private
-spec merkle_tree_node(TreeNodeA :: merkle_tree_node(), TreeNodeB :: merkle_tree_node(), HashFun :: hash_function()) -> merkle_tree_node().
merkle_tree_node(#merkle_tree_node { hash = A } = NodeA, #merkle_tree_node { hash = B } = NodeB, Hash) ->
    HashA = peculium_core_utilities:reverse(A),
    HashB = peculium_core_utilities:reverse(B),
    #merkle_tree_node {
        hash = Hash(<<HashA/binary, HashB/binary>>),
        left = NodeA,
        right = NodeB
    };
merkle_tree_node(A, B, Hash) when is_binary(A), is_binary(B) ->
    HashA = peculium_core_utilities:reverse(A),
    HashB = peculium_core_utilities:reverse(B),
    #merkle_tree_node {
        hash = Hash(<<HashA/binary, HashB/binary>>),
        left = A,
        right = B
    }.

%% @private
-spec merkle_tree_node(Hash :: hash()) -> merkle_tree_node().
merkle_tree_node(Hash) ->
    #merkle_tree_node {
        hash = Hash,
        left = undefined,
        right = undefined
    }.

-ifdef(TEST).

-spec merkle_root_test() -> any().
merkle_root_test() ->
    A = <<140, 20, 240, 219, 61, 241, 80, 18, 62, 111, 61, 187, 243, 15, 139, 149, 90, 130, 73, 182, 42, 193, 209, 255, 22, 40, 74, 239, 163, 208, 109, 135>>,
    B = <<255, 242, 82, 91, 137, 49, 64, 45, 208, 146, 34, 197, 7, 117, 96, 143, 117, 120, 123, 210, 184, 126, 86, 153, 90, 123, 221, 48, 247, 151, 2, 196>>,
    C = <<99, 89, 240, 134, 129, 113, 177, 209, 148, 203, 238, 26, 242, 241, 110, 165, 152, 174, 143, 173, 102, 109, 155, 1, 44, 142, 210, 183, 154, 35, 110, 196>>,
    D = <<233, 166, 104, 69, 224, 93, 90, 188, 10, 208, 78, 200, 15, 119, 74, 126, 88, 92, 110, 141, 185, 117, 150, 45, 6, 154, 82, 33, 55, 184, 12, 29>>,
    R = <<243, 233, 71, 66, 172, 164, 181, 239, 133, 72, 141, 195, 124, 6, 195, 40, 34, 149, 255, 236, 150, 9, 148, 178, 192, 213, 172, 42, 37, 169, 87, 102>>,
    ?assertEqual(hash(from_list([A, B, C, D], fun peculium_core_crypto:hash/1)), R).

-endif.
