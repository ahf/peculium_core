

# Module peculium_inv #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Inv Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Inv objects.
<a name="types"></a>

## Data Types ##




### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_types.md#type-hash">peculium_types:hash()</a>
</code></pre>





### <a name="type-inv">inv()</a> ###



<pre><code>
inv() = <a href="peculium_types.md#type-inv">peculium_types:inv()</a>
</code></pre>





### <a name="type-inv_type">inv_type()</a> ###



<pre><code>
inv_type() = <a href="peculium_types.md#type-inv_type">peculium_types:inv_type()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of a given inv.</td></tr><tr><td valign="top"><a href="#is_block-1">is_block/1</a></td><td>Checks if a given inv is a block.</td></tr><tr><td valign="top"><a href="#is_transaction-1">is_transaction/1</a></td><td>Checks if a given inv is a transaction.</td></tr><tr><td valign="top"><a href="#known-1">known/1</a></td><td>Check if we have the given object.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Returns the type of a given inv.</td></tr><tr><td valign="top"><a href="#unknown-1">unknown/1</a></td><td>Check if we do not have the given object.</td></tr><tr><td valign="top"><a href="#unknown_invs-1">unknown_invs/1</a></td><td>Returns a list of inv objects that we do not currently have.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Inv::<a href="#type-inv">inv()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the hash of a given inv.
<a name="is_block-1"></a>

### is_block/1 ###


<pre><code>
is_block(Inv::<a href="#type-inv">inv()</a>) -&gt; boolean()
</code></pre>

<br></br>


Checks if a given inv is a block.
<a name="is_transaction-1"></a>

### is_transaction/1 ###


<pre><code>
is_transaction(Inv::<a href="#type-inv">inv()</a>) -&gt; boolean()
</code></pre>

<br></br>


Checks if a given inv is a transaction.
<a name="known-1"></a>

### known/1 ###


<pre><code>
known(Inv::<a href="#type-inv">inv()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if we have the given object.
<a name="type-1"></a>

### type/1 ###


<pre><code>
type(Inv::<a href="#type-inv">inv()</a>) -&gt; <a href="#type-inv_type">inv_type()</a>
</code></pre>

<br></br>


Returns the type of a given inv.
<a name="unknown-1"></a>

### unknown/1 ###


<pre><code>
unknown(Inv::<a href="#type-inv">inv()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if we do not have the given object.
<a name="unknown_invs-1"></a>

### unknown_invs/1 ###


<pre><code>
unknown_invs(Invs::[<a href="#type-inv">inv()</a>]) -&gt; [<a href="#type-inv">inv()</a>]
</code></pre>

<br></br>


Returns a list of inv objects that we do not currently have.
