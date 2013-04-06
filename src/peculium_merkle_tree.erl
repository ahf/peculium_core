%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Utilities for creating and interfacing with Merkle tree's.
%% @reference <a href="http://www.google.com/patents?vid=4309569">Merkle tree patent 4,309,569</a>,
%%            <em>Ralph C. Merkle</em>.
-module(peculium_merkle_tree).

%% API.
-export([from_list/2]).
-export([hash/1, left/1, right/1]).

-record(merkle_tree_node, {
    hash :: binary(),
    left :: #merkle_tree_node {} | binary(),
    right :: #merkle_tree_node {} | binary()
}).

-type merkle_tree_node() :: #merkle_tree_node {} | binary().
-type hash_function() :: fun((binary()) -> binary()).

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
from_list([A], Result, Hash) ->
    from_list([A, A], Result, Hash);
from_list([A, B | Rest], Result, Hash) ->
    from_list(Rest, [merkle_tree_node(A, B, Hash) | Result], Hash).

%% @private
-spec merkle_tree_node(merkle_tree_node(), merkle_tree_node(), hash_function()) -> merkle_tree_node().
merkle_tree_node(#merkle_tree_node { hash = HashA } = A, #merkle_tree_node { hash = HashB } = B, Hash) ->
    #merkle_tree_node {
        hash = Hash(<<HashA/binary, HashB/binary>>),
        left = A,
        right = B
    };
merkle_tree_node(A, B, Hash) when is_binary(A), is_binary(B) ->
    #merkle_tree_node {
        hash = Hash(<<A/binary, B/binary>>),
        left = A,
        right = B
    }.
