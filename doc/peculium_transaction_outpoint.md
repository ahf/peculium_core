

# Module peculium_transaction_outpoint #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Transaction Outpoint Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#hash-1">hash/1</a></td><td>Returns the hash of a given outpoint.</td></tr><tr><td valign="top"><a href="#index-1">index/1</a></td><td>Returns the index of a given outpoint.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="hash-1"></a>

### hash/1 ###


<pre><code>
hash(Outpoint::<a href="#type-bitcoin_transaction_outpoint">bitcoin_transaction_outpoint()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the hash of a given outpoint.
<a name="index-1"></a>

### index/1 ###


<pre><code>
index(Outpoint::<a href="#type-bitcoin_transaction_outpoint">bitcoin_transaction_outpoint()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the index of a given outpoint.
