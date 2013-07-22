

# Module peculium_core_difficulty #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Difficulty Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for calculating the Bitcoin difficulty.
<a name="types"></a>

## Data Types ##




### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Calculates the amount of block work from the compact bits representation.</td></tr><tr><td valign="top"><a href="#from_bits-1">from_bits/1</a></td><td>Calculates the difficulty from the compact bits representation.</td></tr><tr><td valign="top"><a href="#target-1">target/1</a></td><td>Calculates the target from the compact bits.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block_work-1"></a>

### block_work/1 ###


<pre><code>
block_work(Bits::<a href="#type-uint32_t">uint32_t()</a>) -&gt; number()
</code></pre>

<br></br>


Calculates the amount of block work from the compact bits representation.
<a name="from_bits-1"></a>

### from_bits/1 ###


<pre><code>
from_bits(Bits::<a href="#type-uint32_t">uint32_t()</a>) -&gt; number()
</code></pre>

<br></br>


Calculates the difficulty from the compact bits representation.
<a name="target-1"></a>

### target/1 ###


<pre><code>
target(Bits::<a href="#type-uint32_t">uint32_t()</a>) -&gt; number()
</code></pre>

<br></br>


Calculates the target from the compact bits.
