

# Module peculium_core_protocol #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Protocol Message Decoder.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-command">command()</a> ###



<pre><code>
command() = <a href="peculium_core_types.md#type-command">peculium_core_types:command()</a>
</code></pre>





### <a name="type-message_types">message_types()</a> ###



<pre><code>
message_types() = <a href="peculium_core_types.md#type-message_types">peculium_core_types:message_types()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_message_payload-2">decode_message_payload/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###


<pre><code>
decode(Data::binary()) -&gt; ok
</code></pre>

<br></br>



<a name="decode_message_payload-2"></a>

### decode_message_payload/2 ###


<pre><code>
decode_message_payload(Command::<a href="#type-command">command()</a>, Data::binary()) -&gt; {ok, <a href="#type-message_types">message_types()</a>} | {error, any()}
</code></pre>

<br></br>



