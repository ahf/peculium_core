

# Module peculium_version #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Version Message Utilities.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains utilities for manipulating and using Bitcoin version
message objects.
<a name="types"></a>

## Data Types ##




### <a name="type-int32_t">int32_t()</a> ###



<pre><code>
int32_t() = <a href="peculium_types.md#type-int32_t">peculium_types:int32_t()</a>
</code></pre>





### <a name="type-int64_t">int64_t()</a> ###



<pre><code>
int64_t() = <a href="peculium_types.md#type-int64_t">peculium_types:int64_t()</a>
</code></pre>





### <a name="type-network_address">network_address()</a> ###



<pre><code>
network_address() = <a href="peculium_types.md#type-network_address">peculium_types:network_address()</a>
</code></pre>





### <a name="type-uint64_t">uint64_t()</a> ###



<pre><code>
uint64_t() = <a href="peculium_types.md#type-uint64_t">peculium_types:uint64_t()</a>
</code></pre>





### <a name="type-version_message">version_message()</a> ###



<pre><code>
version_message() = <a href="peculium_types.md#type-version_message">peculium_types:version_message()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_address-1">from_address/1</a></td><td>Returns the address of the sending client of a given version message.</td></tr><tr><td valign="top"><a href="#nonce-1">nonce/1</a></td><td>Returns the nonce of a given version message.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Returns the service bitset of a given version message.</td></tr><tr><td valign="top"><a href="#start_height-1">start_height/1</a></td><td>Returns the start height of a given version message.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>Returns the timestamp of a given version message.</td></tr><tr><td valign="top"><a href="#to_address-1">to_address/1</a></td><td>Returns the address of the target client of a given version message.</td></tr><tr><td valign="top"><a href="#user_agent-1">user_agent/1</a></td><td>Returns the user agent of a given version message.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given version message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_address-1"></a>

### from_address/1 ###


<pre><code>
from_address(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-network_address">network_address()</a>
</code></pre>

<br></br>


Returns the address of the sending client of a given version message.
<a name="nonce-1"></a>

### nonce/1 ###


<pre><code>
nonce(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the nonce of a given version message.
<a name="services-1"></a>

### services/1 ###


<pre><code>
services(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-uint64_t">uint64_t()</a>
</code></pre>

<br></br>


Returns the service bitset of a given version message.
<a name="start_height-1"></a>

### start_height/1 ###


<pre><code>
start_height(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-int32_t">int32_t()</a>
</code></pre>

<br></br>


Returns the start height of a given version message.
<a name="timestamp-1"></a>

### timestamp/1 ###


<pre><code>
timestamp(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-int64_t">int64_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given version message.
<a name="to_address-1"></a>

### to_address/1 ###


<pre><code>
to_address(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-network_address">network_address()</a>
</code></pre>

<br></br>


Returns the address of the target client of a given version message.
<a name="user_agent-1"></a>

### user_agent/1 ###


<pre><code>
user_agent(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the user agent of a given version message.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(VersionMessage::<a href="#type-version_message">version_message()</a>) -&gt; <a href="#type-int32_t">int32_t()</a>
</code></pre>

<br></br>


Returns the version of a given version message.
