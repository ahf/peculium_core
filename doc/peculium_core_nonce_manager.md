

# Module peculium_core_nonce_manager #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Peculium Nonce Manager
This module contains a `gen_server` for managing connection nonces.
Copyright (c)  2013 Alexander Færøy

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create-0">create/0</a></td><td>Create a new nonce.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete a given nonce.</td></tr><tr><td valign="top"><a href="#has-1">has/1</a></td><td>Check if a given nonce is used by one of our active connections.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start the nonce management server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create-0"></a>

### create/0 ###


<pre><code>
create() -&gt; binary()
</code></pre>

<br></br>


Create a new nonce.
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Nonce::binary()) -&gt; ok
</code></pre>

<br></br>


Delete a given nonce.
<a name="has-1"></a>

### has/1 ###


<pre><code>
has(Nonce::binary()) -&gt; boolean()
</code></pre>

<br></br>


Check if a given nonce is used by one of our active connections.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, any()}
</code></pre>

<br></br>


Start the nonce management server.
