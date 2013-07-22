

# Module peculium_core_transaction_input #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Transaction Input Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Bitcoin
Transaction Input objects.
<a name="types"></a>

## Data Types ##




### <a name="type-transaction_input">transaction_input()</a> ###



<pre><code>
transaction_input() = <a href="peculium_core_types.md#type-transaction_input">peculium_core_types:transaction_input()</a>
</code></pre>





### <a name="type-transaction_outpoint">transaction_outpoint()</a> ###



<pre><code>
transaction_outpoint() = <a href="peculium_core_types.md#type-transaction_outpoint">peculium_core_types:transaction_outpoint()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#previous_output-1">previous_output/1</a></td><td>Returns the previous output of a given transaction input.</td></tr><tr><td valign="top"><a href="#script-1">script/1</a></td><td>Returns the script of a given transaction input.</td></tr><tr><td valign="top"><a href="#sequence-1">sequence/1</a></td><td>Returns the sequence of a given transaction input.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="previous_output-1"></a>

### previous_output/1 ###


<pre><code>
previous_output(TransactionInput::<a href="#type-transaction_input">transaction_input()</a>) -&gt; <a href="#type-transaction_outpoint">transaction_outpoint()</a>
</code></pre>

<br></br>


Returns the previous output of a given transaction input.
<a name="script-1"></a>

### script/1 ###


<pre><code>
script(TransactionInput::<a href="#type-transaction_input">transaction_input()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the script of a given transaction input.
<a name="sequence-1"></a>

### sequence/1 ###


<pre><code>
sequence(TransactionInput::<a href="#type-transaction_input">transaction_input()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the sequence of a given transaction input.
