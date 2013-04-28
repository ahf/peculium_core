

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




### <a name="type-bitcoin_addr_message">bitcoin_addr_message()</a> ###


__abstract datatype__: `bitcoin_addr_message()`




### <a name="type-bitcoin_alert_message">bitcoin_alert_message()</a> ###


__abstract datatype__: `bitcoin_alert_message()`




### <a name="type-bitcoin_block">bitcoin_block()</a> ###


__abstract datatype__: `bitcoin_block()`




### <a name="type-bitcoin_block_header">bitcoin_block_header()</a> ###


__abstract datatype__: `bitcoin_block_header()`




### <a name="type-bitcoin_block_message">bitcoin_block_message()</a> ###


__abstract datatype__: `bitcoin_block_message()`




### <a name="type-bitcoin_checksum">bitcoin_checksum()</a> ###



<pre><code>
bitcoin_checksum() = &lt;&lt;_:32&gt;&gt;
</code></pre>





### <a name="type-bitcoin_command_atom">bitcoin_command_atom()</a> ###



<pre><code>
bitcoin_command_atom() = addr | alert | block | checkorder | getaddr | getblocks | getdata | getheaders | headers | inv | ping | submitorder | reply | tx | verack | version
</code></pre>





### <a name="type-bitcoin_getaddr_message">bitcoin_getaddr_message()</a> ###


__abstract datatype__: `bitcoin_getaddr_message()`




### <a name="type-bitcoin_getblocks_message">bitcoin_getblocks_message()</a> ###


__abstract datatype__: `bitcoin_getblocks_message()`




### <a name="type-bitcoin_getdata_message">bitcoin_getdata_message()</a> ###


__abstract datatype__: `bitcoin_getdata_message()`




### <a name="type-bitcoin_getheaders_message">bitcoin_getheaders_message()</a> ###


__abstract datatype__: `bitcoin_getheaders_message()`




### <a name="type-bitcoin_headers_message">bitcoin_headers_message()</a> ###


__abstract datatype__: `bitcoin_headers_message()`




### <a name="type-bitcoin_inv">bitcoin_inv()</a> ###


__abstract datatype__: `bitcoin_inv()`




### <a name="type-bitcoin_inv_atom">bitcoin_inv_atom()</a> ###



<pre><code>
bitcoin_inv_atom() = error | tx | block
</code></pre>





### <a name="type-bitcoin_inv_integer">bitcoin_inv_integer()</a> ###



<pre><code>
bitcoin_inv_integer() = 0 | 1 | 2
</code></pre>





### <a name="type-bitcoin_inv_message">bitcoin_inv_message()</a> ###


__abstract datatype__: `bitcoin_inv_message()`




### <a name="type-bitcoin_message">bitcoin_message()</a> ###


__abstract datatype__: `bitcoin_message()`




### <a name="type-bitcoin_message_header">bitcoin_message_header()</a> ###


__abstract datatype__: `bitcoin_message_header()`




### <a name="type-bitcoin_network_address">bitcoin_network_address()</a> ###


__abstract datatype__: `bitcoin_network_address()`




### <a name="type-bitcoin_network_atom">bitcoin_network_atom()</a> ###



<pre><code>
bitcoin_network_atom() = mainnet | testnet | testnet3
</code></pre>





### <a name="type-bitcoin_notfound_message">bitcoin_notfound_message()</a> ###


__abstract datatype__: `bitcoin_notfound_message()`




### <a name="type-bitcoin_ping_message">bitcoin_ping_message()</a> ###


__abstract datatype__: `bitcoin_ping_message()`




### <a name="type-bitcoin_transaction">bitcoin_transaction()</a> ###


__abstract datatype__: `bitcoin_transaction()`




### <a name="type-bitcoin_transaction_input">bitcoin_transaction_input()</a> ###


__abstract datatype__: `bitcoin_transaction_input()`




### <a name="type-bitcoin_transaction_outpoint">bitcoin_transaction_outpoint()</a> ###


__abstract datatype__: `bitcoin_transaction_outpoint()`




### <a name="type-bitcoin_transaction_output">bitcoin_transaction_output()</a> ###


__abstract datatype__: `bitcoin_transaction_output()`




### <a name="type-bitcoin_tx_message">bitcoin_tx_message()</a> ###


__abstract datatype__: `bitcoin_tx_message()`




### <a name="type-bitcoin_unit_atom">bitcoin_unit_atom()</a> ###



<pre><code>
bitcoin_unit_atom() = megabitcoin | kilobitcoin | hectobitcoin | decabitcoin | bitcoin | decibitcoin | centibitcoin | millibitcoin | microbitcoin | satoshi
</code></pre>





### <a name="type-bitcoin_verack_message">bitcoin_verack_message()</a> ###


__abstract datatype__: `bitcoin_verack_message()`




### <a name="type-bitcoin_version_message">bitcoin_version_message()</a> ###


__abstract datatype__: `bitcoin_version_message()`




### <a name="type-block_index_entry">block_index_entry()</a> ###


__abstract datatype__: `block_index_entry()`




### <a name="type-block_locator">block_locator()</a> ###



<pre><code>
block_locator() = [binary()]
</code></pre>





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


