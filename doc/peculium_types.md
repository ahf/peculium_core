

# Module peculium_types #
* [Description](#description)
* [Data Types](#types)


Peculium Types.
Copyright (c)  2013 Fearless Hamster Solutions

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).
<a name="description"></a>

## Description ##
   This module contains common types used in the Peculium code.
<a name="types"></a>

## Data Types ##




### <a name="type-addr_message">addr_message()</a> ###


__abstract datatype__: `addr_message()`




### <a name="type-alert_message">alert_message()</a> ###


__abstract datatype__: `alert_message()`




### <a name="type-block">block()</a> ###


__abstract datatype__: `block()`




### <a name="type-block_header">block_header()</a> ###


__abstract datatype__: `block_header()`




### <a name="type-block_index_entry">block_index_entry()</a> ###


__abstract datatype__: `block_index_entry()`




### <a name="type-block_locator">block_locator()</a> ###



<pre><code>
block_locator() = [<a href="#type-hash">hash()</a>]
</code></pre>





### <a name="type-block_message">block_message()</a> ###


__abstract datatype__: `block_message()`




### <a name="type-checksum">checksum()</a> ###



<pre><code>
checksum() = &lt;&lt;_:32&gt;&gt;
</code></pre>





### <a name="type-command_atom">command_atom()</a> ###



<pre><code>
command_atom() = addr | alert | block | checkorder | getaddr | getblocks | getdata | getheaders | headers | inv | ping | submitorder | reply | transaction | verack | version
</code></pre>





### <a name="type-getaddr_message">getaddr_message()</a> ###


__abstract datatype__: `getaddr_message()`




### <a name="type-getblocks_message">getblocks_message()</a> ###


__abstract datatype__: `getblocks_message()`




### <a name="type-getdata_message">getdata_message()</a> ###


__abstract datatype__: `getdata_message()`




### <a name="type-getheaders_message">getheaders_message()</a> ###


__abstract datatype__: `getheaders_message()`




### <a name="type-hash">hash()</a> ###



<pre><code>
hash() = &lt;&lt;_:256&gt;&gt;
</code></pre>





### <a name="type-headers_message">headers_message()</a> ###


__abstract datatype__: `headers_message()`




### <a name="type-int16_t">int16_t()</a> ###



<pre><code>
int16_t() = -32768..32767
</code></pre>





### <a name="type-int32_t">int32_t()</a> ###



<pre><code>
int32_t() = -2147483648..2147483647
</code></pre>





### <a name="type-int64_t">int64_t()</a> ###



<pre><code>
int64_t() = -9223372036854775808..9223372036854775807
</code></pre>





### <a name="type-int8_t">int8_t()</a> ###



<pre><code>
int8_t() = -128..127
</code></pre>





### <a name="type-inv">inv()</a> ###


__abstract datatype__: `inv()`




### <a name="type-inv_atom">inv_atom()</a> ###



<pre><code>
inv_atom() = error | transaction | block
</code></pre>





### <a name="type-inv_integer">inv_integer()</a> ###



<pre><code>
inv_integer() = 0 | 1 | 2
</code></pre>





### <a name="type-inv_message">inv_message()</a> ###


__abstract datatype__: `inv_message()`




### <a name="type-message">message()</a> ###


__abstract datatype__: `message()`




### <a name="type-message_header">message_header()</a> ###


__abstract datatype__: `message_header()`




### <a name="type-message_types">message_types()</a> ###



<pre><code>
message_types() = <a href="#type-verack_message">verack_message()</a> | <a href="#type-ping_message">ping_message()</a> | <a href="#type-getaddr_message">getaddr_message()</a> | <a href="#type-version_message">version_message()</a> | <a href="#type-alert_message">alert_message()</a> | <a href="#type-inv_message">inv_message()</a> | <a href="#type-getdata_message">getdata_message()</a> | <a href="#type-notfound_message">notfound_message()</a> | <a href="#type-addr_message">addr_message()</a> | <a href="#type-headers_message">headers_message()</a> | <a href="#type-getblocks_message">getblocks_message()</a> | <a href="#type-getheaders_message">getheaders_message()</a> | <a href="#type-transaction_message">transaction_message()</a> | <a href="#type-block_message">block_message()</a>
</code></pre>





### <a name="type-network_address">network_address()</a> ###


__abstract datatype__: `network_address()`




### <a name="type-network_atom">network_atom()</a> ###



<pre><code>
network_atom() = mainnet | testnet | testnet3
</code></pre>





### <a name="type-notfound_message">notfound_message()</a> ###


__abstract datatype__: `notfound_message()`




### <a name="type-ping_message">ping_message()</a> ###


__abstract datatype__: `ping_message()`




### <a name="type-transaction">transaction()</a> ###


__abstract datatype__: `transaction()`




### <a name="type-transaction_input">transaction_input()</a> ###


__abstract datatype__: `transaction_input()`




### <a name="type-transaction_message">transaction_message()</a> ###


__abstract datatype__: `transaction_message()`




### <a name="type-transaction_outpoint">transaction_outpoint()</a> ###


__abstract datatype__: `transaction_outpoint()`




### <a name="type-transaction_output">transaction_output()</a> ###


__abstract datatype__: `transaction_output()`




### <a name="type-uint16_t">uint16_t()</a> ###



<pre><code>
uint16_t() = 0..65535
</code></pre>





### <a name="type-uint32_t">uint32_t()</a> ###



<pre><code>
uint32_t() = 0..4294967295
</code></pre>





### <a name="type-uint64_t">uint64_t()</a> ###



<pre><code>
uint64_t() = 0..18446744073709551615
</code></pre>





### <a name="type-uint8_t">uint8_t()</a> ###



<pre><code>
uint8_t() = 0..255
</code></pre>





### <a name="type-unit_atom">unit_atom()</a> ###



<pre><code>
unit_atom() = megabitcoin | kilobitcoin | hectobitcoin | decabitcoin | bitcoin | decibitcoin | centibitcoin | millibitcoin | microbitcoin | satoshi
</code></pre>





### <a name="type-verack_message">verack_message()</a> ###


__abstract datatype__: `verack_message()`




### <a name="type-version_message">version_message()</a> ###


__abstract datatype__: `version_message()`

