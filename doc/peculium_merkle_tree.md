

# Module peculium_merkle_tree #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Merkle Tree Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for creating and traversing Merkle Tree's.
objects.
<a name="types"></a>

## Data Types ##




### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_types.md#type-hash">peculium_types:hash()</a>
</code></pre>





### <a name="type-merkle_tree_node">merkle_tree_node()</a> ###


__abstract datatype__: `merkle_tree_node()`




### <a name="type-transaction">transaction()</a> ###



<pre><code>
transaction() = <a href="peculium_types.md#type-transaction">peculium_types:transaction()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_transactions-1">from_transactions/1</a></td><td>Create a Merkle tree from a list of transactions.</td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Utility function for getting the hash of a given Merkle tree node.</td></tr><tr><td valign="top"><a href="#left-1">left/1</a></td><td>Utility function for getting the left child of a given Merkle tree node.</td></tr><tr><td valign="top"><a href="#right-1">right/1</a></td><td>Utility function for getting the right child of a given Merkle tree node.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_transactions-1"></a>

### from_transactions/1 ###


<pre><code>
from_transactions(Transactions::[<a href="#type-transaction">transaction()</a>, ...]) -&gt; <a href="#type-merkle_tree_node">merkle_tree_node()</a>
</code></pre>

<br></br>


Create a Merkle tree from a list of transactions.
This function takes a list of transactions and returns the root node of the
Merkle tree.
<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(TreeNode::<a href="#type-merkle_tree_node">merkle_tree_node()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Utility function for getting the hash of a given Merkle tree node.
<a name="left-1"></a>

### left/1 ###


<pre><code>
left(TreeNode::<a href="#type-merkle_tree_node">merkle_tree_node()</a>) -&gt; <a href="#type-merkle_tree_node">merkle_tree_node()</a>
</code></pre>

<br></br>


Utility function for getting the left child of a given Merkle tree node.
<a name="right-1"></a>

### right/1 ###


<pre><code>
right(TreeNode::<a href="#type-merkle_tree_node">merkle_tree_node()</a>) -&gt; <a href="#type-merkle_tree_node">merkle_tree_node()</a>
</code></pre>

<br></br>


Utility function for getting the right child of a given Merkle tree node.
