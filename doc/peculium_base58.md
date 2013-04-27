

# Module peculium_base58 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Base58 encoding and decoding utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

__References__* [
Base58Check encoding
](https://en.bitcoin.it/wiki/Base58Check_encoding)
----------------------------------------------------------------------------

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decode a Base58 binary.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a binary using Base58.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(X::binary()) -&gt; {ok, binary()} | {error, {invalid_byte, binary()}}
</code></pre>

<br></br>


Decode a Base58 binary.
<a name="encode-1"></a>

### encode/1 ###


<pre><code>
encode(X::binary()) -&gt; binary()
</code></pre>

<br></br>


Encode a binary using Base58.
