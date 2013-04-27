

# Module peculium_block_index_entry #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Block Index Types and Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-1">block/1</a></td><td>Returns the block of a given block index entry.</td></tr><tr><td valign="top"><a href="#block_header-1">block_header/1</a></td><td>Returns the block header of the given block index entry.</td></tr><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Calculates the amount of work in the current block.</td></tr><tr><td valign="top"><a href="#from_block-1">from_block/1</a></td><td>Create a new block index entry from a Block.</td></tr><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of the block that the given block index entry points to.</td></tr><tr><td valign="top"><a href="#height-1">height/1</a></td><td>Returns the height of a given block index entry.</td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td>Returns the hash of the next block of a given block index entry.</td></tr><tr><td valign="top"><a href="#next_index-1">next_index/1</a></td><td>Returns the block index entry of the next block of a given block index entry.</td></tr><tr><td valign="top"><a href="#previous-1">previous/1</a></td><td>Returns the hash of the previous block of a given block index entry.</td></tr><tr><td valign="top"><a href="#previous_index-1">previous_index/1</a></td><td>Returns the block index entry of the previous block of a given block index entry.</td></tr><tr><td valign="top"><a href="#total_chain_work-1">total_chain_work/1</a></td><td>Returns the total amount of work in the chain including the work in this block.</td></tr><tr><td valign="top"><a href="#total_transaction_count-1">total_transaction_count/1</a></td><td>Returns the total number of transactions in the block chain including this block.</td></tr><tr><td valign="top"><a href="#transaction_count-1">transaction_count/1</a></td><td>Returns the transaction count of the given block index entry.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block-1"></a>

### block/1 ###


<pre><code>
block(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; <a href="#type-bitcoin_block">bitcoin_block()</a>
</code></pre>

<br></br>


Returns the block of a given block index entry.
<a name="block_header-1"></a>

### block_header/1 ###


<pre><code>
block_header(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; <a href="#type-bitcoin_block_header">bitcoin_block_header()</a>
</code></pre>

<br></br>


Returns the block header of the given block index entry.
<a name="block_work-1"></a>

### block_work/1 ###


<pre><code>
block_work(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; float()
</code></pre>

<br></br>


Calculates the amount of work in the current block.
<a name="from_block-1"></a>

### from_block/1 ###


<pre><code>
from_block(Block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; <a href="#type-block_index_entry">block_index_entry()</a>
</code></pre>

<br></br>


Create a new block index entry from a Block.
<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the hash of the block that the given block index entry points to.
<a name="height-1"></a>

### height/1 ###


<pre><code>
height(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


Returns the height of a given block index entry.
<a name="next-1"></a>

### next/1 ###


<pre><code>
next(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; binary() | undefined
</code></pre>

<br></br>


Returns the hash of the next block of a given block index entry.
<a name="next_index-1"></a>

### next_index/1 ###


<pre><code>
next_index(BlockIndexEntry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; <a href="#type-block_index_entry">block_index_entry()</a> | undefined
</code></pre>

<br></br>


Returns the block index entry of the next block of a given block index entry.
<a name="previous-1"></a>

### previous/1 ###


<pre><code>
previous(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; binary() | undefined
</code></pre>

<br></br>


Returns the hash of the previous block of a given block index entry.
<a name="previous_index-1"></a>

### previous_index/1 ###


<pre><code>
previous_index(BlockIndexEntry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; <a href="#type-block_index_entry">block_index_entry()</a> | undefined
</code></pre>

<br></br>


Returns the block index entry of the previous block of a given block index entry.
<a name="total_chain_work-1"></a>

### total_chain_work/1 ###


<pre><code>
total_chain_work(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


Returns the total amount of work in the chain including the work in this block.
<a name="total_transaction_count-1"></a>

### total_transaction_count/1 ###


<pre><code>
total_transaction_count(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


Returns the total number of transactions in the block chain including this block.
<a name="transaction_count-1"></a>

### transaction_count/1 ###


<pre><code>
transaction_count(Block_index_entry::<a href="#type-block_index_entry">block_index_entry()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>


Returns the transaction count of the given block index entry.
