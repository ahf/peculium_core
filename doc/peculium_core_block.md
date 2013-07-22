

# Module peculium_core_block #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Block Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and generating Bitcoin
block objects.
<a name="types"></a>

## Data Types ##




### <a name="type-block">block()</a> ###



<pre><code>
block() = <a href="peculium_core_types.md#type-block">peculium_core_types:block()</a>
</code></pre>





### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_core_types.md#type-hash">peculium_core_types:hash()</a>
</code></pre>





### <a name="type-network">network()</a> ###



<pre><code>
network() = <a href="peculium_core_types.md#type-network">peculium_core_types:network()</a>
</code></pre>





### <a name="type-transaction">transaction()</a> ###



<pre><code>
transaction() = <a href="peculium_core_types.md#type-transaction">peculium_core_types:transaction()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bits-1">bits/1</a></td><td>Returns the bits of a given block.</td></tr><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Returns the block work of a given block.</td></tr><tr><td valign="top"><a href="#difficulty-1">difficulty/1</a></td><td>Returns the difficulty of a given block.</td></tr><tr><td valign="top"><a href="#genesis_block-1">genesis_block/1</a></td><td>Returns the Genesis block from a given network.</td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the little-endian encoded hash of a given block.</td></tr><tr><td valign="top"><a href="#merkle_root-1">merkle_root/1</a></td><td>Returns the root hash of the merkle tree of a given block.</td></tr><tr><td valign="top"><a href="#previous-1">previous/1</a></td><td>Returns the hash of the previous block of a given block.</td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td>Returns the target of a given block.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>Returns the timestamp of a given block.</td></tr><tr><td valign="top"><a href="#transactions-1">transactions/1</a></td><td>Returns a list of transactions of a given block.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given block.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bits-1"></a>

### bits/1 ###


<pre><code>
bits(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the bits of a given block.
<a name="block_work-1"></a>

### block_work/1 ###


<pre><code>
block_work(Block::<a href="#type-block">block()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the block work of a given block.
<a name="difficulty-1"></a>

### difficulty/1 ###


<pre><code>
difficulty(Block::<a href="#type-block">block()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the difficulty of a given block.
<a name="genesis_block-1"></a>

### genesis_block/1 ###


<pre><code>
genesis_block(Network::<a href="#type-network">network()</a>) -&gt; <a href="#type-block">block()</a>
</code></pre>

<br></br>


Returns the Genesis block from a given network.
<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the little-endian encoded hash of a given block.
<a name="merkle_root-1"></a>

### merkle_root/1 ###


<pre><code>
merkle_root(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the root hash of the merkle tree of a given block.
<a name="previous-1"></a>

### previous/1 ###


<pre><code>
previous(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the hash of the previous block of a given block.
<a name="target-1"></a>

### target/1 ###


<pre><code>
target(Block::<a href="#type-block">block()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the target of a given block.
<a name="timestamp-1"></a>

### timestamp/1 ###


<pre><code>
timestamp(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given block.
<a name="transactions-1"></a>

### transactions/1 ###


<pre><code>
transactions(Block::<a href="#type-block">block()</a>) -&gt; [<a href="#type-transaction">transaction()</a>]
</code></pre>

<br></br>


Returns a list of transactions of a given block.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Block::<a href="#type-block">block()</a>) -&gt; integer()
</code></pre>

<br></br>


Returns the version of a given block.
