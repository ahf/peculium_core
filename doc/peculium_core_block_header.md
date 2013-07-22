

# Module peculium_core_block_header #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Block Header Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Block Header
objects.
<a name="types"></a>

## Data Types ##




### <a name="type-block">block()</a> ###



<pre><code>
block() = <a href="peculium_core_types.md#type-block">peculium_core_types:block()</a>
</code></pre>





### <a name="type-block_header">block_header()</a> ###



<pre><code>
block_header() = <a href="peculium_core_types.md#type-block_header">peculium_core_types:block_header()</a>
</code></pre>





### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_core_types.md#type-hash">peculium_core_types:hash()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>





### <a name="type-uint8_t">uint8_t()</a> ###



<pre><code>
uint8_t() = <a href="peculium_core_types.md#type-uint8_t">peculium_core_types:uint8_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bits-1">bits/1</a></td><td>Returns the bits of a given block header.</td></tr><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Returns the block work for the given block header.</td></tr><tr><td valign="top"><a href="#difficulty-1">difficulty/1</a></td><td>Returns the difficulty for the given block header.</td></tr><tr><td valign="top"><a href="#from_block-1">from_block/1</a></td><td>Create Bitcoin Block Header from a given Block.</td></tr><tr><td valign="top"><a href="#merkle_root-1">merkle_root/1</a></td><td>Returns the root hash of the merkle tree of a given block header.</td></tr><tr><td valign="top"><a href="#nonce-1">nonce/1</a></td><td>Returns the nonce of a given block header.</td></tr><tr><td valign="top"><a href="#previous-1">previous/1</a></td><td>Returns the hash of the previous block of a given block header.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>Returns the timestamp of a given block header.</td></tr><tr><td valign="top"><a href="#transaction_count-1">transaction_count/1</a></td><td>Returns the transaction count (always 0) of a given block header.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given block header.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bits-1"></a>

### bits/1 ###


<pre><code>
bits(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the bits of a given block header.
<a name="block_work-1"></a>

### block_work/1 ###


<pre><code>
block_work(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the block work for the given block header.
<a name="difficulty-1"></a>

### difficulty/1 ###


<pre><code>
difficulty(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; number()
</code></pre>

<br></br>


Returns the difficulty for the given block header.
<a name="from_block-1"></a>

### from_block/1 ###


<pre><code>
from_block(Block::<a href="#type-block">block()</a>) -&gt; <a href="#type-block_header">block_header()</a>
</code></pre>

<br></br>


Create Bitcoin Block Header from a given Block.
<a name="merkle_root-1"></a>

### merkle_root/1 ###


<pre><code>
merkle_root(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the root hash of the merkle tree of a given block header.
<a name="nonce-1"></a>

### nonce/1 ###


<pre><code>
nonce(Blockheader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the nonce of a given block header.
<a name="previous-1"></a>

### previous/1 ###


<pre><code>
previous(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the hash of the previous block of a given block header.
<a name="timestamp-1"></a>

### timestamp/1 ###


<pre><code>
timestamp(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given block header.
<a name="transaction_count-1"></a>

### transaction_count/1 ###


<pre><code>
transaction_count(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-uint8_t">uint8_t()</a>
</code></pre>

<br></br>


Returns the transaction count (always 0) of a given block header.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(BlockHeader::<a href="#type-block_header">block_header()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the version of a given block header.
