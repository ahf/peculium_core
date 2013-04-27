

# Module peculium_block_store #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Block Store Server.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete block.</td></tr><tr><td valign="top"><a href="#exists-1">exists/1</a></td><td>Check if a given block exists in the store.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get block.</td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td>Insert block.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start Block Store Server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Hash::binary()) -&gt; ok
</code></pre>

<br></br>


Delete block.
<a name="exists-1"></a>

### exists/1 ###


<pre><code>
exists(Hash::binary()) -&gt; boolean()
</code></pre>

<br></br>


Check if a given block exists in the store.
<a name="get-1"></a>

### get/1 ###


<pre><code>
get(Hash::binary()) -&gt; {ok, <a href="#type-bitcoin_block">bitcoin_block()</a>} | {error, not_found}
</code></pre>

<br></br>


Get block.
<a name="put-2"></a>

### put/2 ###


<pre><code>
put(Hash::binary(), Block::<a href="#type-bitcoin_block">bitcoin_block()</a>) -&gt; ok
</code></pre>

<br></br>


Insert block.
<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

Start Block Store Server.
