

# Module peculium_protocol_types #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


       Bitcoin Protocol Type Encoders and Decoders.
__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   ----------------------------------------------------------------------------<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-1">block/1</a></td><td></td></tr><tr><td valign="top"><a href="#block_header-1">block_header/1</a></td><td></td></tr><tr><td valign="top"><a href="#bool-1">bool/1</a></td><td></td></tr><tr><td valign="top"><a href="#int16_t-1">int16_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#int32_t-1">int32_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#int64_t-1">int64_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#int8_t-1">int8_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#inv-1">inv/1</a></td><td></td></tr><tr><td valign="top"><a href="#net_addr-1">net_addr/1</a></td><td></td></tr><tr><td valign="top"><a href="#net_addr-2">net_addr/2</a></td><td></td></tr><tr><td valign="top"><a href="#net_addr-3">net_addr/3</a></td><td></td></tr><tr><td valign="top"><a href="#transaction-1">transaction/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction_input-1">transaction_input/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction_outpoint-1">transaction_outpoint/1</a></td><td></td></tr><tr><td valign="top"><a href="#transaction_output-1">transaction_output/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint16_t-1">uint16_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint32_t-1">uint32_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint64_t-1">uint64_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#uint8_t-1">uint8_t/1</a></td><td></td></tr><tr><td valign="top"><a href="#var_int-1">var_int/1</a></td><td></td></tr><tr><td valign="top"><a href="#var_string-1">var_string/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="block-1"></a>

### block/1 ###

`block(Bitcoin_block) -> any()`


<a name="block_header-1"></a>

### block_header/1 ###


<pre><code>
block_header(X1::binary()) -&gt; {ok, <a href="#type-bitcoin_block_header">bitcoin_block_header()</a>}
</code></pre>

<br></br>



<a name="bool-1"></a>

### bool/1 ###


<pre><code>
bool(X::<a href="#type-uint8_t">uint8_t()</a>) -&gt; boolean()
</code></pre>

<br></br>



<a name="int16_t-1"></a>

### int16_t/1 ###


<pre><code>
int16_t(X::<a href="#type-int16_t">int16_t()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="int32_t-1"></a>

### int32_t/1 ###


<pre><code>
int32_t(X::<a href="#type-int32_t">int32_t()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="int64_t-1"></a>

### int64_t/1 ###


<pre><code>
int64_t(X::<a href="#type-int64_t">int64_t()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="int8_t-1"></a>

### int8_t/1 ###


<pre><code>
int8_t(X::<a href="#type-int8_t">int8_t()</a>) -&gt; integer()
</code></pre>

<br></br>



<a name="inv-1"></a>

### inv/1 ###


<pre><code>
inv(Bitcoin_inv::binary()) -&gt; {ok, <a href="#type-bitcoin_inv">bitcoin_inv()</a>}
</code></pre>

<br></br>



<a name="net_addr-1"></a>

### net_addr/1 ###


<pre><code>
net_addr(X1::binary()) -&gt; {ok, <a href="#type-bitcoin_network_address">bitcoin_network_address()</a>}
</code></pre>

<br></br>



<a name="net_addr-2"></a>

### net_addr/2 ###

`net_addr(Address, Port) -> any()`


<a name="net_addr-3"></a>

### net_addr/3 ###

`net_addr(Timestamp, Address, Port) -> any()`


<a name="transaction-1"></a>

### transaction/1 ###

`transaction(Bitcoin_transaction) -> any()`


<a name="transaction_input-1"></a>

### transaction_input/1 ###


<pre><code>
transaction_input(Bitcoin_transaction_input::binary()) -&gt; {ok, <a href="#type-bitcoin_transaction_input">bitcoin_transaction_input()</a>}
</code></pre>

<br></br>



<a name="transaction_outpoint-1"></a>

### transaction_outpoint/1 ###


<pre><code>
transaction_outpoint(Bitcoin_transaction_outpoint::binary()) -&gt; {ok, <a href="#type-bitcoin_transaction_outpoint">bitcoin_transaction_outpoint()</a>}
</code></pre>

<br></br>



<a name="transaction_output-1"></a>

### transaction_output/1 ###


<pre><code>
transaction_output(Bitcoin_transaction_output::binary()) -&gt; {ok, <a href="#type-bitcoin_transaction_output">bitcoin_transaction_output()</a>}
</code></pre>

<br></br>



<a name="uint16_t-1"></a>

### uint16_t/1 ###


<pre><code>
uint16_t(X::<a href="#type-uint16_t">uint16_t()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



<a name="uint32_t-1"></a>

### uint32_t/1 ###


<pre><code>
uint32_t(X::<a href="#type-uint32_t">uint32_t()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



<a name="uint64_t-1"></a>

### uint64_t/1 ###


<pre><code>
uint64_t(X::<a href="#type-uint64_t">uint64_t()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



<a name="uint8_t-1"></a>

### uint8_t/1 ###


<pre><code>
uint8_t(X::<a href="#type-uint8_t">uint8_t()</a>) -&gt; non_neg_integer()
</code></pre>

<br></br>



<a name="var_int-1"></a>

### var_int/1 ###


<pre><code>
var_int(X::binary()) -&gt; {ok, integer(), binary()} | {error, {invalid_var_int, any()}}
</code></pre>

<br></br>



<a name="var_string-1"></a>

### var_string/1 ###


<pre><code>
var_string(X::binary()) -&gt; {ok, binary(), binary()} | {error, any()}
</code></pre>

<br></br>



