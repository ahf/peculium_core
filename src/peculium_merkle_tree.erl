%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Utilities for creating and interfacing with Merkle tree's.
%% @reference <a href="http://www.google.com/patents?vid=4309569">Merkle tree patent 4,309,569</a>,
%%            <em>Ralph C. Merkle</em>.
-module(peculium_merkle_tree).

%% API.
-export([from_transactions/1, from_list/2]).
-export([hash/1, left/1, right/1]).

-include_lib("peculium/include/peculium.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(merkle_tree_node, {
    hash :: binary(),
    left :: #merkle_tree_node {} | binary() | undefined,
    right :: #merkle_tree_node {} | binary() | undefined
}).

-type merkle_tree_node() :: #merkle_tree_node {} | binary() | undefined.
-type hash_function() :: fun((binary()) -> binary()).

%% @doc Create a Merkle tree from a list of transactions.
%% This function takes a list of transactions and returns the root node of the
%% Merkle tree.
%% @end
-spec from_transactions([bitcoin_tx_message(), ...]) -> merkle_tree_node().
from_transactions(Transactions) ->
    from_list(lists:map(fun peculium_transaction:hash/1, Transactions), fun (X) -> peculium_utilities:reverse(crypto:sha256(crypto:sha256(X))) end).

%% @doc Create a Merkle Tree from a list of leaves.
%% This function takes a list of leaves and a hash function and returns the
%% root node of the Merkle tree.
%% @end
-spec from_list([binary(), ...], hash_function()) -> merkle_tree_node().
from_list(Hashes, Hash) when is_list(Hashes) ->
    from_list(Hashes, [], Hash).

%% @doc Utility function for reading the hash of a given Merkle tree node.
-spec hash(merkle_tree_node()) -> binary().
hash(#merkle_tree_node { hash = Hash }) ->
    Hash.

%% @doc Utility function for reading the left child of a given Merkle tree node.
-spec left(merkle_tree_node()) -> merkle_tree_node().
left(#merkle_tree_node { left = LeftChild }) ->
    LeftChild.

%% @doc Utility function for reading the right child of a given Merkle tree node.
-spec right(merkle_tree_node()) -> merkle_tree_node().
right(#merkle_tree_node { right = RightChild }) ->
    RightChild.

%% @private
-spec from_list([binary(), ...], [merkle_tree_node()], hash_function()) -> merkle_tree_node().
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
-spec merkle_tree_node(merkle_tree_node(), merkle_tree_node(), hash_function()) -> merkle_tree_node().
merkle_tree_node(#merkle_tree_node { hash = A } = NodeA, #merkle_tree_node { hash = B } = NodeB, Hash) ->
    HashA = peculium_utilities:reverse(A),
    HashB = peculium_utilities:reverse(B),
    #merkle_tree_node {
        hash = Hash(<<HashA/binary, HashB/binary>>),
        left = NodeA,
        right = NodeB
    };
merkle_tree_node(A, B, Hash) when is_binary(A), is_binary(B) ->
    HashA = peculium_utilities:reverse(A),
    HashB = peculium_utilities:reverse(B),
    #merkle_tree_node {
        hash = Hash(<<HashA/binary, HashB/binary>>),
        left = A,
        right = B
    }.

%% @private
-spec merkle_tree_node(binary()) -> merkle_tree_node().
merkle_tree_node(A) ->
    #merkle_tree_node {
        hash = A,
        left = undefined,
        right = undefined
    }.

-ifdef(TEST).

-spec test() -> any().

-spec merkle_root_test() -> any().
merkle_root_test() ->
    A = <<140, 20, 240, 219, 61, 241, 80, 18, 62, 111, 61, 187, 243, 15, 139, 149, 90, 130, 73, 182, 42, 193, 209, 255, 22, 40, 74, 239, 163, 208, 109, 135>>,
    B = <<255, 242, 82, 91, 137, 49, 64, 45, 208, 146, 34, 197, 7, 117, 96, 143, 117, 120, 123, 210, 184, 126, 86, 153, 90, 123, 221, 48, 247, 151, 2, 196>>,
    C = <<99, 89, 240, 134, 129, 113, 177, 209, 148, 203, 238, 26, 242, 241, 110, 165, 152, 174, 143, 173, 102, 109, 155, 1, 44, 142, 210, 183, 154, 35, 110, 196>>,
    D = <<233, 166, 104, 69, 224, 93, 90, 188, 10, 208, 78, 200, 15, 119, 74, 126, 88, 92, 110, 141, 185, 117, 150, 45, 6, 154, 82, 33, 55, 184, 12, 29>>,
    R = <<243, 233, 71, 66, 172, 164, 181, 239, 133, 72, 141, 195, 124, 6, 195, 40, 34, 149, 255, 236, 150, 9, 148, 178, 192, 213, 172, 42, 37, 169, 87, 102>>,
    ?assertEqual(hash(from_list([A, B, C, D], fun (X) -> peculium_utilities:reverse(crypto:sha256(crypto:sha256(X))) end)), R).

-endif.
