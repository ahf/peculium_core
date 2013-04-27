

# Module peculium_transaction_input #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Transaction Input Utilities.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#previous_output-1">previous_output/1</a></td><td>Returns the previous output of a given transaction input.</td></tr><tr><td valign="top"><a href="#script-1">script/1</a></td><td>Returns the script of a given transaction input.</td></tr><tr><td valign="top"><a href="#sequence-1">sequence/1</a></td><td>Returns the sequence of a given transaction input.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="previous_output-1"></a>

### previous_output/1 ###


<pre><code>
previous_output(TransactionInput::<a href="#type-bitcoin_transaction_input">bitcoin_transaction_input()</a>) -&gt; <a href="#type-bitcoin_transaction_outpoint">bitcoin_transaction_outpoint()</a>
</code></pre>

<br></br>


Returns the previous output of a given transaction input.
<a name="script-1"></a>

### script/1 ###


<pre><code>
script(TransactionInput::<a href="#type-bitcoin_transaction_input">bitcoin_transaction_input()</a>) -&gt; binary()
</code></pre>

<br></br>


Returns the script of a given transaction input.
<a name="sequence-1"></a>

### sequence/1 ###


<pre><code>
sequence(TransactionInput::<a href="#type-bitcoin_transaction_input">bitcoin_transaction_input()</a>) -&gt; <a href="#type-uint32_t">uint32_t()</a>
</code></pre>

<br></br>


Returns the sequence of a given transaction input.
