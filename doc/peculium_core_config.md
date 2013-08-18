

# Module peculium_core_config #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


       Peculium's Configuration server.
__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------
<a name="types"></a>

## Data Types ##




### <a name="type-network">network()</a> ###



<pre><code>
network() = <a href="peculium_core_types.md#type-network">peculium_core_types:network()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block_chain_dir-0">block_chain_dir/0</a></td><td>Get the block chain directory.</td></tr><tr><td valign="top"><a href="#block_index_dir-0">block_index_dir/0</a></td><td>Get the block index directory.</td></tr><tr><td valign="top"><a href="#block_store_dir-0">block_store_dir/0</a></td><td>Get the block store directory.</td></tr><tr><td valign="top"><a href="#bootstrap-0">bootstrap/0</a></td><td>Bootstrap methods.</td></tr><tr><td valign="top"><a href="#cache_size-0">cache_size/0</a></td><td>Get the cache size in bytes.</td></tr><tr><td valign="top"><a href="#data_dir-0">data_dir/0</a></td><td>Get the configuration directory.</td></tr><tr><td valign="top"><a href="#mnesia_dir-0">mnesia_dir/0</a></td><td>Get the Mnesia directory.</td></tr><tr><td valign="top"><a href="#network-0">network/0</a></td><td>Get the network.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start the configuration server.</td></tr><tr><td valign="top"><a href="#use_upnp-0">use_upnp/0</a></td><td>Use UPnP.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block_chain_dir-0"></a>

### block_chain_dir/0 ###


<pre><code>
block_chain_dir() -&gt; string()
</code></pre>

<br></br>


Get the block chain directory.
<a name="block_index_dir-0"></a>

### block_index_dir/0 ###


<pre><code>
block_index_dir() -&gt; string()
</code></pre>

<br></br>


Get the block index directory.
<a name="block_store_dir-0"></a>

### block_store_dir/0 ###


<pre><code>
block_store_dir() -&gt; string()
</code></pre>

<br></br>


Get the block store directory.
<a name="bootstrap-0"></a>

### bootstrap/0 ###


<pre><code>
bootstrap() -&gt; [term()]
</code></pre>

<br></br>


Bootstrap methods.
<a name="cache_size-0"></a>

### cache_size/0 ###


<pre><code>
cache_size() -&gt; non_neg_integer()
</code></pre>

<br></br>


Get the cache size in bytes.
<a name="data_dir-0"></a>

### data_dir/0 ###


<pre><code>
data_dir() -&gt; string()
</code></pre>

<br></br>


Get the configuration directory.
<a name="mnesia_dir-0"></a>

### mnesia_dir/0 ###


<pre><code>
mnesia_dir() -&gt; string()
</code></pre>

<br></br>


Get the Mnesia directory.
<a name="network-0"></a>

### network/0 ###


<pre><code>
network() -&gt; <a href="#type-network">network()</a>
</code></pre>

<br></br>


Get the network.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start the configuration server.
<a name="use_upnp-0"></a>

### use_upnp/0 ###


<pre><code>
use_upnp() -&gt; boolean()
</code></pre>

<br></br>


Use UPnP.
