

# Module peculium_core_base58 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Base58 Encoding and Decoding Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##


Base58 is used in the Bitcoin stack to encode public and private keys in
human-typable strings. For instance, a Bitcoin address is a hash of the
public key with a checksum appended and then converted to Base58 encoding.



The original Satoshi client source code discusses the reasoning behind the
Base58 encoding as the following:



- Avoid 0, O, I and l characters as they look the same in certain fonts and
could be used to trick people into transferring Bitcoins to the wrong
address.



- A string with non-alphanumeric characters is not easily accepted as an
account number.



- An email client usually won't add a line-break unless there's punctuation
to break it.


- Double clicking selects the whole word and not just a section of the
word.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decode Base58 data.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode data using Base58.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Data::binary()) -&gt; {ok, binary()} | {error, {invalid_byte, binary()}}
</code></pre>

<br></br>


Decode Base58 data.
<a name="encode-1"></a>

### encode/1 ###


<pre><code>
encode(Data::binary()) -&gt; binary()
</code></pre>

<br></br>


Encode data using Base58.
