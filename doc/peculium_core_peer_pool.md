

# Module peculium_core_peer_pool #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Peer Pool.
Copyright (c)  2013 Alexander Færøy

__Behaviours:__ [`supervisor`](supervisor.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains a `supervisor` for supervising `peculium_core_peer`
servers.
<a name="types"></a>

## Data Types ##




### <a name="type-startlink_err">startlink_err()</a> ###



<pre><code>
startlink_err() = {already_started, pid()} | shutdown | term()
</code></pre>





### <a name="type-startlink_ret">startlink_ret()</a> ###



<pre><code>
startlink_ret() = {ok, pid()} | ignore | {error, <a href="#type-startlink_err">startlink_err()</a>}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_peer-2">spawn_peer/2</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_peer-4">spawn_peer/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###


<pre><code>
init(State::[]) -&gt; {ok, {{simple_one_for_one, non_neg_integer(), non_neg_integer()}, []}}
</code></pre>

<br></br>



<a name="spawn_peer-2"></a>

### spawn_peer/2 ###


<pre><code>
spawn_peer(Address::<a href="inet.md#type-ip_address">inet:ip_address()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; {ok, pid()} | {error, term()}
</code></pre>

<br></br>



<a name="spawn_peer-4"></a>

### spawn_peer/4 ###


<pre><code>
spawn_peer(ListenerPid::pid(), Socket::<a href="inet.md#type-socket">inet:socket()</a>, Transport::term(), Options::[term()]) -&gt; {ok, pid()} | {error, term()}
</code></pre>

<br></br>



<a name="start_link-0"></a>

### start_link/0 ###


<pre><code>
start_link() -&gt; <a href="#type-startlink_ret">startlink_ret()</a>
</code></pre>

<br></br>



