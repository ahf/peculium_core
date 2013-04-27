

# Module peculium_version #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Version Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_address-1">from_address/1</a></td><td>Returns the address of the sending client of a given version message.</td></tr><tr><td valign="top"><a href="#nonce-1">nonce/1</a></td><td>Returns the nonce of a given version message.</td></tr><tr><td valign="top"><a href="#services-1">services/1</a></td><td>Returns the service bitset of a given version message.</td></tr><tr><td valign="top"><a href="#start_height-1">start_height/1</a></td><td>Returns the start height of a given version message.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>Returns the timestamp of a given version message.</td></tr><tr><td valign="top"><a href="#to_address-1">to_address/1</a></td><td>Returns the address of the target client of a given version message.</td></tr><tr><td valign="top"><a href="#user_agent-1">user_agent/1</a></td><td>Returns the user agent of a given version message.</td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td>Returns the version of a given version message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_address-1"></a>

### from_address/1 ###


<pre><code>
from_address(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-bitcoin_network_address">bitcoin_network_address()</a>
</code></pre>

<br></br>


Returns the address of the sending client of a given version message.
<a name="nonce-1"></a>

### nonce/1 ###


<pre><code>
nonce(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the nonce of a given version message.
<a name="services-1"></a>

### services/1 ###


<pre><code>
services(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-uint64_t">uint64_t()</a>
</code></pre>

<br></br>


Returns the service bitset of a given version message.
<a name="start_height-1"></a>

### start_height/1 ###


<pre><code>
start_height(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-int32_t">int32_t()</a>
</code></pre>

<br></br>


Returns the start height of a given version message.
<a name="timestamp-1"></a>

### timestamp/1 ###


<pre><code>
timestamp(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-int64_t">int64_t()</a>
</code></pre>

<br></br>


Returns the timestamp of a given version message.
<a name="to_address-1"></a>

### to_address/1 ###


<pre><code>
to_address(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-bitcoin_network_address">bitcoin_network_address()</a>
</code></pre>

<br></br>


Returns the address of the target client of a given version message.
<a name="user_agent-1"></a>

### user_agent/1 ###


<pre><code>
user_agent(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the user agent of a given version message.
<a name="version-1"></a>

### version/1 ###


<pre><code>
version(Bitcoin_version_message::<a href="#type-bitcoin_version_message">bitcoin_version_message()</a>) -&gt; <a href="#type-int32_t">int32_t()</a>
</code></pre>

<br></br>


Returns the version of a given version message.
