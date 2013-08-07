

# Module peculium_core_messages #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin message encoders.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##


This module is used by the peculium_core_peer process to create Bitcoin
messages.
<a name="types"></a>

## Data Types ##




### <a name="type-block_locator">block_locator()</a> ###



<pre><code>
block_locator() = <a href="peculium_core_types.md#type-block_locator">peculium_core_types:block_locator()</a>
</code></pre>





### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_core_types.md#type-hash">peculium_core_types:hash()</a>
</code></pre>





### <a name="type-inv">inv()</a> ###



<pre><code>
inv() = <a href="peculium_core_types.md#type-inv">peculium_core_types:inv()</a>
</code></pre>





### <a name="type-network">network()</a> ###



<pre><code>
network() = <a href="peculium_core_types.md#type-network">peculium_core_types:network()</a>
</code></pre>





### <a name="type-transaction">transaction()</a> ###



<pre><code>
transaction() = <a href="peculium_core_types.md#type-transaction">peculium_core_types:transaction()</a>
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = <a href="peculium_core_types.md#type-uint32_t">peculium_core_types:uint32_t()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-8">block/8</a></td><td>Create block message in the network wire format.</td></tr><tr><td valign="top"><a href="#getaddr-1">getaddr/1</a></td><td>Create getaddr message in the network wire format.</td></tr><tr><td valign="top"><a href="#getblocks-3">getblocks/3</a></td><td>Create getblocks message in the network wire format.</td></tr><tr><td valign="top"><a href="#getdata-2">getdata/2</a></td><td>Create getdata message in the network wire format.</td></tr><tr><td valign="top"><a href="#getheaders-3">getheaders/3</a></td><td>Create getheaders message in the network wire format.</td></tr><tr><td valign="top"><a href="#ping-1">ping/1</a></td><td>Create ping message in the network wire format.</td></tr><tr><td valign="top"><a href="#verack-1">verack/1</a></td><td>Create verack message in the network wire format.</td></tr><tr><td valign="top"><a href="#version-6">version/6</a></td><td>Create version message in the network wire format.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block-8"></a>

### block/8 ###


<pre><code>
block(Network::<a href="#type-network">network()</a>, Version::<a href="#type-uint32_t">uint32_t()</a>, PreviousBlock::<a href="#type-hash">hash()</a>, MerkleRoot::<a href="#type-hash">hash()</a>, Timestamp::non_neg_integer(), Bits::binary(), Nonce::binary(), Transactions::[<a href="#type-transaction">transaction()</a>]) -&gt; iolist()
</code></pre>

<br></br>


Create block message in the network wire format.
<a name="getaddr-1"></a>

### getaddr/1 ###


<pre><code>
getaddr(Network::<a href="#type-network">network()</a>) -&gt; iolist()
</code></pre>

<br></br>


Create getaddr message in the network wire format.
<a name="getblocks-3"></a>

### getblocks/3 ###


<pre><code>
getblocks(Network::<a href="#type-network">network()</a>, BlockLocator::<a href="#type-block_locator">block_locator()</a>, BlockStop::<a href="#type-hash">hash()</a>) -&gt; iolist()
</code></pre>

<br></br>


Create getblocks message in the network wire format.
<a name="getdata-2"></a>

### getdata/2 ###


<pre><code>
getdata(Network::<a href="#type-network">network()</a>, Invs::[<a href="#type-inv">inv()</a>]) -&gt; iolist()
</code></pre>

<br></br>


Create getdata message in the network wire format.
<a name="getheaders-3"></a>

### getheaders/3 ###


<pre><code>
getheaders(Network::<a href="#type-network">network()</a>, BlockLocator::<a href="#type-block_locator">block_locator()</a>, BlockStop::<a href="#type-hash">hash()</a>) -&gt; iolist()
</code></pre>

<br></br>


Create getheaders message in the network wire format.
<a name="ping-1"></a>

### ping/1 ###


<pre><code>
ping(Network::<a href="#type-network">network()</a>) -&gt; iolist()
</code></pre>

<br></br>


Create ping message in the network wire format.
<a name="verack-1"></a>

### verack/1 ###


<pre><code>
verack(Network::<a href="#type-network">network()</a>) -&gt; iolist()
</code></pre>

<br></br>


Create verack message in the network wire format.
<a name="version-6"></a>

### version/6 ###


<pre><code>
version(Network::<a href="#type-network">network()</a>, SourceAddress::<a href="inet.md#type-ip_address">inet:ip_address()</a>, SourcePort::<a href="inet.md#type-port_number">inet:port_number()</a>, DestinationAddress::<a href="inet.md#type-ip_address">inet:ip_address()</a>, DestinationPort::<a href="inet.md#type-port_number">inet:port_number()</a>, Nonce::binary()) -&gt; iolist()
</code></pre>

<br></br>


Create version message in the network wire format.
