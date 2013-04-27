

# Module peculium_difficulty #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Difficulty Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

__References__* [
Bitcoin Difficulty
](https://en.bitcoin.it/wiki/Difficulty)
----------------------------------------------------------------------------

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block_work-1">block_work/1</a></td><td>Calculates the amount of block work from the compact bits representation.</td></tr><tr><td valign="top"><a href="#from_bits-1">from_bits/1</a></td><td>Calculates the difficulty from the compact bits representation.</td></tr></table>


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
