

# Module peculium_network_address #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Network Address Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Network Address
objects.
<a name="types"></a>

## Data Types ##




### <a name="type-network_address">network_address()</a> ###



<pre><code>
network_address() = <a href="peculium_types.md#type-network_address">peculium_types:network_address()</a>
</code></pre>





### <a name="type-uint16_t">uint16_t()</a> ###



<pre><code>
uint16_t() = <a href="peculium_types.md#type-uint16_t">peculium_types:uint16_t()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_types.md#type-uint32_t">peculium_types:uint32_t()</a>
</code></pre>





### <a name="type-uint64_t">uint64_t()</a> ###



<pre><code>
uint64_t() = <a href="peculium_types.md#type-uint64_t">peculium_types:uint64_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#address-1">address/1</a></td><td>Returns the address of a given network address.</td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Returns the port of a given network address.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Returns the services value of a given network address.</td></tr><tr><td valign="top"><a href="#time-1">time/1</a></td><td>Returns the timestamp of a given network address.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="address-1"></a>

### address/1 ###


<pre><code>
address(NetworkAddress::<a href="#type-network_address">network_address()</a>) -&gt; <a href="inet.md#type-ip6_address">inet:ip6_address()</a>
</code></pre>

<br></br>


Returns the address of a given network address.
<a name="port-1"></a>

### port/1 ###


<pre><code>
port(NetworkAddress::<a href="#type-network_address">network_address()</a>) -&gt; <a href="#type-uint16_t">uint16_t()</a>
</code></pre>

<br></br>


Returns the port of a given network address.
<a name="services-1"></a>

### services/1 ###


<pre><code>
services(NetworkAddress::<a href="#type-network_address">network_address()</a>) -&gt; <a href="#type-uint64_t">uint64_t()</a>
</code></pre>

<br></br>


Returns the services value of a given network address.
<a name="time-1"></a>

### time/1 ###


<pre><code>
time(NetworkAddress::<a href="#type-network_address">network_address()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given network address.
