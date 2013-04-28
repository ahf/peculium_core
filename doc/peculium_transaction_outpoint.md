

# Module peculium_transaction_outpoint #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Transaction Outpoint Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Bitcoin
Transaction Outpoint objects.
<a name="types"></a>

## Data Types ##




### <a name="type-transaction_outpoint">transaction_outpoint()</a> ###



<pre><code>
transaction_outpoint() = <a href="peculium_types.md#type-transaction_outpoint">peculium_types:transaction_outpoint()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_types.md#type-uint32_t">peculium_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of a given outpoint.</td></tr><tr><td valign="top"><a href="#index-1">index/1</a></td><td>Returns the index of a given outpoint.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Outpoint::<a href="#type-transaction_outpoint">transaction_outpoint()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the hash of a given outpoint.
<a name="index-1"></a>

### index/1 ###


<pre><code>
index(Outpoint::<a href="#type-transaction_outpoint">transaction_outpoint()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the index of a given outpoint.
