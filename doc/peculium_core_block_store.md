

# Module peculium_core_block_store #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Bitcoin Block Store Server.
Copyright (c)  2013 Alexander Færøy

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains a `gen_server` server for storing and retrieving
Bitcoin block's.
<a name="types"></a>

## Data Types ##




### <a name="type-block">block()</a> ###



<pre><code>
block() = <a href="peculium_core_types.md#type-block">peculium_core_types:block()</a>
</code></pre>





### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = <a href="peculium_core_types.md#type-hash">peculium_core_types:hash()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete block.</td></tr><tr><td valign="top"><a href="#exists-1">exists/1</a></td><td>Check if a given block exists in the store.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get block.</td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td>Insert block.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start Block Store Server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Hash::<a href="#type-hash">hash()</a>) -&gt; ok
</code></pre>

<br></br>


Delete block.
<a name="exists-1"></a>

### exists/1 ###


<pre><code>
exists(Hash::<a href="#type-hash">hash()</a>) -&gt; boolean()
</code></pre>

<br></br>


Check if a given block exists in the store.
<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Hash::<a href="#type-hash">hash()</a>) -&gt; {ok, <a href="#type-block">block()</a>} | {error, not_found}
</code></pre>

<br></br>


Get block.
<a name="put-2"></a>

### put/2 ###


<pre><code>
put(Hash::<a href="#type-hash">hash()</a>, Block::<a href="#type-block">block()</a>) -&gt; ok
</code></pre>

<br></br>


Insert block.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start Block Store Server.
