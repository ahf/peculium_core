

# Module peculium_transaction #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Transaction Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Bitcoin
Transaction objects.
<a name="types"></a>

## Data Types ##




### <a name="type-bitcoin_transaction">bitcoin_transaction()</a> ###



<pre><code>
bitcoin_transaction() = <a href="peculium_types.md#type-bitcoin_transaction">peculium_types:bitcoin_transaction()</a>
</code></pre>





### <a name="type-bitcoin_transaction_input">bitcoin_transaction_input()</a> ###



<pre><code>
bitcoin_transaction_input() = <a href="peculium_types.md#type-bitcoin_transaction_input">peculium_types:bitcoin_transaction_input()</a>
</code></pre>





### <a name="type-bitcoin_transaction_output">bitcoin_transaction_output()</a> ###



<pre><code>
bitcoin_transaction_output() = <a href="peculium_types.md#type-bitcoin_transaction_output">peculium_types:bitcoin_transaction_output()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_types.md#type-uint32_t">peculium_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of a given transaction.</td></tr><tr><td valign="top"><a href="#inputs-1">inputs/1</a></td><td>Returns the transaction inputs of a given transaction.</td></tr><tr><td valign="top"><a href="#is_coinbase-1">is_coinbase/1</a></td><td>Check if a given transaction is a coinbase transaction.</td></tr><tr><td valign="top"><a href="#lock_time-1">lock_time/1</a></td><td>Returns the lock time of a given transaction.</td></tr><tr><td valign="top"><a href="#outputs-1">outputs/1</a></td><td>Returns the transaction outputs of a given transaction.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given transaction.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the hash of a given transaction.
<a name="inputs-1"></a>

### inputs/1 ###


<pre><code>
inputs(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; [<a href="#type-bitcoin_transaction_input">bitcoin_transaction_input()</a>]
</code></pre>

<br></br>


Returns the transaction inputs of a given transaction.
<a name="is_coinbase-1"></a>

### is_coinbase/1 ###


<pre><code>
is_coinbase(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if a given transaction is a coinbase transaction.
<a name="lock_time-1"></a>

### lock_time/1 ###


<pre><code>
lock_time(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the lock time of a given transaction.
<a name="outputs-1"></a>

### outputs/1 ###


<pre><code>
outputs(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; [<a href="#type-bitcoin_transaction_output">bitcoin_transaction_output()</a>]
</code></pre>

<br></br>


Returns the transaction outputs of a given transaction.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Transaction::<a href="#type-bitcoin_transaction">bitcoin_transaction()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the version of a given transaction.
