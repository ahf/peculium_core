

# Module peculium_core_peer_nonce_manager #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_nonce-0">create_nonce/0</a></td><td>Create a new nonce.</td></tr><tr><td valign="top"><a href="#has_nonce-1">has_nonce/1</a></td><td>Check if a given nonce is used by one of our active connections.</td></tr><tr><td valign="top"><a href="#remove_nonce-1">remove_nonce/1</a></td><td>Delete a given nonce.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Start the nonce management server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_nonce-0"></a>

### create_nonce/0 ###


<pre><code>
create_nonce() -&gt; binary()
</code></pre>

<br></br>


Create a new nonce.
<a name="has_nonce-1"></a>

### has_nonce/1 ###


<pre><code>
has_nonce(Nonce::binary()) -&gt; boolean()
</code></pre>

<br></br>


Check if a given nonce is used by one of our active connections.
<a name="remove_nonce-1"></a>

### remove_nonce/1 ###


<pre><code>
remove_nonce(Nonce::binary()) -&gt; ok
</code></pre>

<br></br>


Delete a given nonce.
<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, any()}
</code></pre>

<br></br>


Start the nonce management server.
