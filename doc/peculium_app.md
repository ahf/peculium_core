

# Module peculium_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       The Peculium Application.
__Behaviours:__ [`application`](application.md).

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-0"></a>

### start/0 ###


<pre><code>
start() -&gt; ok | {error, term()}
</code></pre>

<br></br>



<a name="start-2"></a>

### start/2 ###


<pre><code>
start(X1::normal | {takeover, node()} | {failover, node()}, X2::term()) -&gt; {ok, pid()}
</code></pre>

<br></br>



<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(State::[]) -&gt; ok
</code></pre>

<br></br>



