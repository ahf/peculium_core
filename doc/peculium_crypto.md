

# Module peculium_crypto #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Cryptography Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_types.md#type-hash">peculium_types:hash()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the double SHA256 checksum of a given input.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Data::iolist()) -&gt; <a href="#type-hash">hash()</a>
</code></pre>

<br></br>


Returns the double SHA256 checksum of a given input.
