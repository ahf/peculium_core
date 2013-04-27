

# Module peculium_network_address #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Network Address Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#address-1">address/1</a></td><td>Returns the address of a given network address.</td></tr><tr><td valign="top"><a href="#port-1">port/1</a></td><td>Returns the port of a given network address.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Returns the services value of a given network address.</td></tr><tr><td valign="top"><a href="#time-1">time/1</a></td><td>Returns the timestamp of a given network address.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="address-1"></a>

### address/1 ###


<pre><code>
address(NetworkAddress::<a href="#type-bitcoin_network_address">bitcoin_network_address()</a>) -&gt; <a href="inet.md#type-ip6_address">inet:ip6_address()</a>
</code></pre>

<br></br>


Returns the address of a given network address.
<a name="port-1"></a>

### port/1 ###


<pre><code>
port(NetworkAddress::<a href="#type-bitcoin_network_address">bitcoin_network_address()</a>) -&gt; <a href="#type-uint16_t">uint16_t()</a>
</code></pre>

<br></br>


Returns the port of a given network address.
<a name="services-1"></a>

### services/1 ###


<pre><code>
services(NetworkAddress::<a href="#type-bitcoin_network_address">bitcoin_network_address()</a>) -&gt; <a href="#type-uint64_t">uint64_t()</a>
</code></pre>

<br></br>


Returns the services value of a given network address.
<a name="time-1"></a>

### time/1 ###


<pre><code>
time(NetworkAddress::<a href="#type-bitcoin_network_address">bitcoin_network_address()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given network address.
