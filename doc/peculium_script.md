

# Module peculium_script #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Script Decoder.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-script">script()</a> ###



<pre><code>
script() = <a href="peculium_types.md#type-script">peculium_types:script()</a>
</code></pre>





### <a name="type-script_op">script_op()</a> ###



<pre><code>
script_op() = <a href="peculium_types.md#type-script_op">peculium_types:script_op()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#contains_disabled_opcodes-1">contains_disabled_opcodes/1</a></td><td>Check if a given script contains disabled opcodes.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Try to decode a given binary containing script opcodes.</td></tr><tr><td valign="top"><a href="#is_disabled_opcode-1">is_disabled_opcode/1</a></td><td>Check if a given script opcode is disabled.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="contains_disabled_opcodes-1"></a>

### contains_disabled_opcodes/1 ###


<pre><code>
contains_disabled_opcodes(Script::<a href="#type-script">script()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if a given script contains disabled opcodes.
<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Opcodes::binary()) -&gt; {ok, <a href="#type-script">script()</a>} | {error, any()}
</code></pre>

<br></br>


Try to decode a given binary containing script opcodes.
<a name="is_disabled_opcode-1"></a>

### is_disabled_opcode/1 ###


<pre><code>
is_disabled_opcode(Opcode::<a href="#type-script_op">script_op()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if a given script opcode is disabled.
