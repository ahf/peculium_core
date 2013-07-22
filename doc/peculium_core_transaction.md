

# Module peculium_core_transaction #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Transaction Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Bitcoin
Transaction objects.
<a name="types"></a>

## Data Types ##




### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_core_types.md#type-hash">peculium_core_types:hash()</a>
</code></pre>





### <a name="type-transaction">transaction()</a> ###



<pre><code>
transaction() = <a href="peculium_core_types.md#type-transaction">peculium_core_types:transaction()</a>
</code></pre>





### <a name="type-transaction_input">transaction_input()</a> ###



<pre><code>
transaction_input() = <a href="peculium_core_types.md#type-transaction_input">peculium_core_types:transaction_input()</a>
</code></pre>





### <a name="type-transaction_output">transaction_output()</a> ###



<pre><code>
transaction_output() = <a href="peculium_core_types.md#type-transaction_output">peculium_core_types:transaction_output()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of a given transaction.</td></tr><tr><td valign="top"><a href="#inputs-1">inputs/1</a></td><td>Returns the transaction inputs of a given transaction.</td></tr><tr><td valign="top"><a href="#is_coinbase-1">is_coinbase/1</a></td><td>Check if a given transaction is a coinbase transaction.</td></tr><tr><td valign="top"><a href="#lock_time-1">lock_time/1</a></td><td>Returns the lock time of a given transaction.</td></tr><tr><td valign="top"><a href="#outputs-1">outputs/1</a></td><td>Returns the transaction outputs of a given transaction.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given transaction.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the hash of a given transaction.
<a name="inputs-1"></a>

### inputs/1 ###


<pre><code>
inputs(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; [<a href="#type-transaction_input">transaction_input()</a>]
</code></pre>

<br></br>


Returns the transaction inputs of a given transaction.
<a name="is_coinbase-1"></a>

### is_coinbase/1 ###


<pre><code>
is_coinbase(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if a given transaction is a coinbase transaction.
<a name="lock_time-1"></a>

### lock_time/1 ###


<pre><code>
lock_time(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the lock time of a given transaction.
<a name="outputs-1"></a>

### outputs/1 ###


<pre><code>
outputs(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; [<a href="#type-transaction_output">transaction_output()</a>]
</code></pre>

<br></br>


Returns the transaction outputs of a given transaction.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Transaction::<a href="#type-transaction">transaction()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the version of a given transaction.
