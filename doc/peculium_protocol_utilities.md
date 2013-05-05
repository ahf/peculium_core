

# Module peculium_protocol_utilities #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Protocol Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities used in the Bitcoin protocol encoder and
decoder.
<a name="types"></a>

## Data Types ##




### <a name="type-checksum">checksum()</a> ###



<pre><code>
checksum() = <a href="peculium_types.md#type-checksum">peculium_types:checksum()</a>
</code></pre>





### <a name="type-command">command()</a> ###



<pre><code>
command() = <a href="peculium_types.md#type-command">peculium_types:command()</a>
</code></pre>





### <a name="type-dynamic_vector_decode_fun">dynamic_vector_decode_fun()</a> ###



<pre><code>
dynamic_vector_decode_fun() = fun((Data::binary()) -&gt; {ok, Item::any(), Rest::binary()} | {error, any()})
</code></pre>





### <a name="type-inv_integer">inv_integer()</a> ###



<pre><code>
inv_integer() = <a href="peculium_types.md#type-inv_integer">peculium_types:inv_integer()</a>
</code></pre>





### <a name="type-inv_type">inv_type()</a> ###



<pre><code>
inv_type() = <a href="peculium_types.md#type-inv_type">peculium_types:inv_type()</a>
</code></pre>





### <a name="type-vector_decode_fun">vector_decode_fun()</a> ###



<pre><code>
vector_decode_fun() = fun((Data::binary()) -&gt; {ok, Item::any()} | {error, any()})
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom_to_inv-1">atom_to_inv/1</a></td><td>Returns an integer from a given inv type.</td></tr><tr><td valign="top"><a href="#checksum-1">checksum/1</a></td><td>Returns the first four bytes of the double SHA256 checksum of the given Data.</td></tr><tr><td valign="top"><a href="#command_to_atom-1">command_to_atom/1</a></td><td>Returns a command atom from a given binary.</td></tr><tr><td valign="top"><a href="#decode_dynamic_vector-3">decode_dynamic_vector/3</a></td><td>Decode a vector where the size of each item is unknown.</td></tr><tr><td valign="top"><a href="#decode_vector-3">decode_vector/3</a></td><td>Decode a vector where the size of each item is known.</td></tr><tr><td valign="top"><a href="#inv_to_atom-1">inv_to_atom/1</a></td><td>Returns an inv atom from a given integer.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="atom_to_inv-1"></a>

### atom_to_inv/1 ###


<pre><code>
atom_to_inv(Inv::<a href="#type-inv_type">inv_type()</a>) -&gt; {ok, <a href="#type-inv_integer">inv_integer()</a>} | {error, any()}
</code></pre>

<br></br>


Returns an integer from a given inv type.
<a name="checksum-1"></a>

### checksum/1 ###


<pre><code>
checksum(Data::iolist()) -&gt; <a href="#type-checksum">checksum()</a>
</code></pre>

<br></br>


Returns the first four bytes of the double SHA256 checksum of the given Data.
<a name="command_to_atom-1"></a>

### command_to_atom/1 ###


<pre><code>
command_to_atom(Command::binary()) -&gt; {ok, <a href="#type-command">command()</a>} | {error, any()}
</code></pre>

<br></br>


Returns a command atom from a given binary.
<a name="decode_dynamic_vector-3"></a>

### decode_dynamic_vector/3 ###


<pre><code>
decode_dynamic_vector(Data::binary(), ItemCount::non_neg_integer(), ItemDecodeFun::<a href="#type-dynamic_vector_decode_fun">dynamic_vector_decode_fun()</a>) -&gt; {ok, [any()], binary()} | {error, any()}
</code></pre>

<br></br>



Decode a vector where the size of each item is unknown.
The Bitcoin protocol uses vectors where each element size is unknown until
the time of item decoding.


This function takes a decode function that consumes the bytes needed to
decode an item and returns the decoded item together with the rest of the
bytes. This is applied recursively to the remaining bytes until we have
decoded `ItemCount` number of items or if an error occurs.
<a name="decode_vector-3"></a>

### decode_vector/3 ###


<pre><code>
decode_vector(Data::binary(), ItemSize::non_neg_integer(), ItemDecodeFun::<a href="#type-vector_decode_fun">vector_decode_fun()</a>) -&gt; {ok, [any()], binary()} | {error, any()}
</code></pre>

<br></br>


Decode a vector where the size of each item is known.
<a name="inv_to_atom-1"></a>

### inv_to_atom/1 ###


<pre><code>
inv_to_atom(InvInteger::integer()) -&gt; {ok, <a href="#type-inv_type">inv_type()</a>} | {error, any()}
</code></pre>

<br></br>


Returns an inv atom from a given integer.
