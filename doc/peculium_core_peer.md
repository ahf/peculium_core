

# Module peculium_core_peer #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Peer Server.
Copyright (c)  2013 Alexander Færøy

__Behaviours:__ [`gen_server`](gen_server.md), [`ranch_protocol`](ranch_protocol.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##

   This module contains a `gen_server` for representing a peer in the Bitcoin
peer-to-peer network.


We are using a single server to represent both incoming and outgoing
peers.
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





### <a name="type-peer">peer()</a> ###



<pre><code>
peer() = pid()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-8">block/8</a></td><td>Send block message to the given Peer.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#connect-3">connect/3</a></td><td></td></tr><tr><td valign="top"><a href="#getaddr-1">getaddr/1</a></td><td>Send getaddr message to the given Peer.</td></tr><tr><td valign="top"><a href="#getblocks-3">getblocks/3</a></td><td>Send getblocks message to the given Peer.</td></tr><tr><td valign="top"><a href="#getdata-2">getdata/2</a></td><td>Send getdata message to the given Peer.</td></tr><tr><td valign="top"><a href="#getheaders-3">getheaders/3</a></td><td>Send getheaders message to the given Peer.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#ping-1">ping/1</a></td><td>Send ping message to the given Peer.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start Peer server.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the given Peer server.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#verack-1">verack/1</a></td><td>Send verack message to the given Peer.</td></tr><tr><td valign="top"><a href="#version-2">version/2</a></td><td>Send version message to the given Peer.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block-8"></a>

### block/8 ###


<pre><code>
block(Peer::<a href="#type-peer">peer()</a>, Version::<a href="#type-uint32_t">uint32_t()</a>, PreviousBlock::<a href="#type-hash">hash()</a>, MerkleRoot::<a href="#type-hash">hash()</a>, Timestamp::non_neg_integer(), Bits::binary(), Nonce::binary(), Transactions::[<a href="#type-transaction">transaction()</a>]) -&gt; ok
</code></pre>

<br></br>


Send block message to the given Peer.
<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`


<a name="connect-3"></a>

### connect/3 ###

`connect(Peer, Address, Port) -> any()`


<a name="getaddr-1"></a>

### getaddr/1 ###


<pre><code>
getaddr(Peer::<a href="#type-peer">peer()</a>) -&gt; ok
</code></pre>

<br></br>


Send getaddr message to the given Peer.
<a name="getblocks-3"></a>

### getblocks/3 ###


<pre><code>
getblocks(Peer::<a href="#type-peer">peer()</a>, BlockLocator::<a href="#type-block_locator">block_locator()</a>, BlockStop::<a href="#type-hash">hash()</a>) -&gt; ok
</code></pre>

<br></br>


Send getblocks message to the given Peer.
<a name="getdata-2"></a>

### getdata/2 ###


<pre><code>
getdata(Peer::<a href="#type-peer">peer()</a>, Invs::[<a href="#type-inv">inv()</a>]) -&gt; ok
</code></pre>

<br></br>


Send getdata message to the given Peer.
<a name="getheaders-3"></a>

### getheaders/3 ###


<pre><code>
getheaders(Peer::<a href="#type-peer">peer()</a>, BlockLocator::<a href="#type-block_locator">block_locator()</a>, BlockStop::<a href="#type-hash">hash()</a>) -&gt; ok
</code></pre>

<br></br>


Send getheaders message to the given Peer.
<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`


<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Message, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`


<a name="init-1"></a>

### init/1 ###


<pre><code>
init(Arguments::[any()]) -&gt; {ok, term()} | {ok, term(), non_neg_integer() | infinity} | {ok, term(), hibernate} | {stop, any()} | ignore
</code></pre>

<br></br>



<a name="ping-1"></a>

### ping/1 ###


<pre><code>
ping(Peer::<a href="#type-peer">peer()</a>) -&gt; ok
</code></pre>

<br></br>


Send ping message to the given Peer.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, <a href="#type-peer">peer()</a>} | ignore | {error, any()}
</code></pre>

<br></br>


Start Peer server.
<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(Peer::<a href="#type-peer">peer()</a>) -&gt; ok
</code></pre>

<br></br>


Stop the given Peer server.
<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


<a name="verack-1"></a>

### verack/1 ###


<pre><code>
verack(Peer::<a href="#type-peer">peer()</a>) -&gt; ok
</code></pre>

<br></br>


Send verack message to the given Peer.
<a name="version-2"></a>

### version/2 ###


<pre><code>
version(Peer::<a href="#type-peer">peer()</a>, Nonce::binary()) -&gt; ok
</code></pre>

<br></br>


Send version message to the given Peer.
