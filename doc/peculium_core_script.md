

# Module peculium_core_script #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Script Decoder.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-script">script()</a> ###



<pre><code>
script() = <a href="peculium_core_types.md#type-script">peculium_core_types:script()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Try to decode a given binary containing script opcodes.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Opcodes::binary()) -&gt; {ok, <a href="#type-script">script()</a>} | {error, any()}
</code></pre>

<br></br>


Try to decode a given binary containing script opcodes.
