

# Module peculium_transaction_output #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Transaction Output Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using transaction
output objects.
<a name="types"></a>

## Data Types ##




### <a name="type-bitcoin_transaction_output">bitcoin_transaction_output()</a> ###



<pre><code>
bitcoin_transaction_output() = <a href="peculium_types.md#type-bitcoin_transaction_output">peculium_types:bitcoin_transaction_output()</a>
</code></pre>





### <a name="type-int64_t">int64_t()</a> ###



<pre><code>
int64_t() = <a href="peculium_types.md#type-int64_t">peculium_types:int64_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#script-1">script/1</a></td><td>Returns the script of a given transaction output.</td></tr><tr><td valign="top"><a href="#value-1">value/1</a></td><td>Returns the value of a given transaction output.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="script-1"></a>

### script/1 ###


<pre><code>
script(TransactionOutput::<a href="#type-bitcoin_transaction_output">bitcoin_transaction_output()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the script of a given transaction output.
<a name="value-1"></a>

### value/1 ###


<pre><code>
value(TransactionOutput::<a href="#type-bitcoin_transaction_output">bitcoin_transaction_output()</a>) -&gt; <a href="#type-int64_t">int64_t()</a>
</code></pre>

<br></br>


Returns the value of a given transaction output.
