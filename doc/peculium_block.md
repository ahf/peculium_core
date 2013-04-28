

# Module peculium_block #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Block Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Returns the block work for the given block.</td></tr><tr><td valign="top"><a href="#difficulty-1">difficulty/1</a></td><td>Returns the difficulty for the given block.</td></tr><tr><td valign="top"><a href="#genesis_block-1">genesis_block/1</a></td><td>Returns the Genesis block from a given network.</td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the little-endian encoded hash of a given block.</td></tr><tr><td valign="top"><a href="#merkle_root-1">merkle_root/1</a></td><td>Returns the root hash of the merkle tree of a given block.</td></tr><tr><td valign="top"><a href="#previous-1">previous/1</a></td><td>Returns the hash of the previous block of a given block.</td></tr><tr><td valign="top"><a href="#transactions-1">transactions/1</a></td><td>Returns a list of transactions from a given block.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given block.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block_work-1"></a>

### block_work/1 ###


<pre><code>
block_work(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the block work for the given block.
<a name="difficulty-1"></a>

### difficulty/1 ###


<pre><code>
difficulty(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the difficulty for the given block.
<a name="genesis_block-1"></a>

### genesis_block/1 ###


<pre><code>
genesis_block(X1::<a href="#type-bitcoin_network_atom">bitcoin_network_atom()</a>) -&gt; <a href="#type-bitcoin_block">bitcoin_block()</a>
</code></pre>

<br></br>


Returns the Genesis block from a given network.
<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the little-endian encoded hash of a given block.
<a name="merkle_root-1"></a>

### merkle_root/1 ###


<pre><code>
merkle_root(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the root hash of the merkle tree of a given block.
<a name="previous-1"></a>

### previous/1 ###


<pre><code>
previous(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the hash of the previous block of a given block.
<a name="transactions-1"></a>

### transactions/1 ###


<pre><code>
transactions(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; [<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>]
</code></pre>

<br></br>


Returns a list of transactions from a given block.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Bitcoin_block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; integer()
</code></pre>

<br></br>


Returns the version of a given block.
