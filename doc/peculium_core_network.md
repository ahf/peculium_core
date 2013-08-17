

# Module peculium_core_network #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Network Utilities.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-network">network()</a> ###



<pre><code>
network() = <a href="peculium_core_types.md#type-network">peculium_core_types:network()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#magic_value-1">magic_value/1</a></td><td>Get network wire magical value from a given network.</td></tr><tr><td valign="top"><a href="#port_number-1">port_number/1</a></td><td>Get default port from a given network.</td></tr><tr><td valign="top"><a href="#stringify-1">stringify/1</a></td><td>Convert a given network atom to a binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="magic_value-1"></a>

### magic_value/1 ###


<pre><code>
magic_value(Network::<a href="#type-network">network()</a>) -&gt; {ok, binary()} | {error, term()}
</code></pre>

<br></br>


Get network wire magical value from a given network.
<a name="port_number-1"></a>

### port_number/1 ###


<pre><code>
port_number(Network::<a href="#type-network">network()</a>) -&gt; {ok, Port::<a href="inet.md#type-port_number">inet:port_number()</a>} | {error, term()}
</code></pre>

<br></br>


Get default port from a given network.
<a name="stringify-1"></a>

### stringify/1 ###


<pre><code>
stringify(Network::<a href="#type-network">network()</a>) -&gt; {ok, binary()} | {error, term()}
</code></pre>

<br></br>


Convert a given network atom to a binary.
