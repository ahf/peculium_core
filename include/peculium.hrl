%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%

-define(PECULIUM_VERSION, <<"0.1.0">>).
-define(PECULIUM_USER_AGENT, [<<"peculium/", ?PECULIUM_VERSION>>]).

-type bitcoin_unit_atom() :: megabitcoin | kilobitcoin | hectobitcoin | decabitcoin
                           | bitcoin | decibitcoin | centibitcoin | millibitcoin
                           | microbitcoin | satoshi.

-type uint8_t()  :: 0..255.
-type uint16_t() :: 0..65535.
-type uint32_t() :: 0..4294967295.
-type uint64_t() :: 0..18446744073709551615.

-type int8_t()  :: integer().
-type int16_t() :: integer().
-type int32_t() :: integer().
-type int64_t() :: integer().

-type bitcoin_network_atom() :: mainnet | testnet | testnet3.

-type bitcoin_command_atom() :: addr | alert | block | checkorder
                              | getaddr | getblocks | getdata | getheaders
                              | headers | inv | ping | submitorder
                              | reply | tx | verack | version.

-type bitcoin_inv_atom() :: error | tx | block.
-type bitcoin_inv_integer() :: 0 | 1 | 2.

-record(bitcoin_inv, {
    type :: bitcoin_inv_atom(),
    hash :: binary()
}).

-type bitcoin_inv() :: #bitcoin_inv {}.

-type bitcoin_checksum() :: <<_:32>>.

-record(bitcoin_outpoint, {
    index :: uint32_t(),
    hash :: binary()
}).

-type bitcoin_outpoint() :: #bitcoin_outpoint {}.

-record(bitcoin_transaction_input, {
    previous_output :: bitcoin_outpoint(),
    script :: binary(),
    sequence :: uint32_t()
}).

-type bitcoin_transaction_input() :: #bitcoin_transaction_input {}.

-record(bitcoin_transaction_output, {
    value :: int64_t(),
    script :: binary()
}).

-type bitcoin_transaction_output() :: #bitcoin_transaction_output {}.

-record(bitcoin_net_address, {
    time :: uint32_t(),
    services :: uint64_t(),
    address :: inet:ip6_address(),
    port :: uint16_t()
}).

-type bitcoin_net_address() :: #bitcoin_net_address {}.

-record(bitcoin_block_header, {
    version :: uint32_t(),
    previous_block :: binary(),
    merkle_root :: binary(),
    timestamp :: uint32_t(),
    bits :: uint32_t(),
    nonce :: uint32_t(),
    transaction_count :: uint8_t()
}).

-type bitcoin_block_header() :: #bitcoin_block_header {}.

-record(bitcoin_message_header, {
    network :: bitcoin_network_atom(),
    command :: bitcoin_command_atom(),
    length :: uint32_t(),
    checksum :: bitcoin_checksum(),
    valid :: boolean()
}).

-type bitcoin_message_header() :: #bitcoin_message_header {}.

-record(bitcoin_verack_message, {}).

-type bitcoin_verack_message() :: #bitcoin_verack_message {}.

-record(bitcoin_ping_message, {}).

-type bitcoin_ping_message() :: #bitcoin_ping_message {}.

-record(bitcoin_getaddr_message, {}).

-type bitcoin_getaddr_message() :: #bitcoin_getaddr_message {}.

-record(bitcoin_version_message, {
    version :: int32_t(),
    services :: uint64_t(),
    timestamp :: int64_t(),
    to_address :: bitcoin_net_address(),
    from_address :: bitcoin_net_address(),
    user_agent :: binary(),
    start_height :: int32_t(),
    relay :: boolean(),
    nonce :: binary()
}).

-type bitcoin_version_message() :: #bitcoin_version_message {}.

-record(bitcoin_alert_message, {
    payload :: binary(),
    signature :: binary()
}).

-type bitcoin_alert_message() :: #bitcoin_alert_message {}.

-record(bitcoin_inv_message, {
    inventory :: [bitcoin_inv()]
}).

-type bitcoin_inv_message() :: #bitcoin_inv_message {}.

-record(bitcoin_getdata_message, {
    inventory :: [bitcoin_inv()]
}).

-type bitcoin_getdata_message() :: #bitcoin_getdata_message {}.

-record(bitcoin_notfound_message, {
    inventory :: [bitcoin_inv()]
}).

-type bitcoin_notfound_message() :: #bitcoin_notfound_message {}.

-record(bitcoin_addr_message, {
    addresses :: [bitcoin_net_address()]
}).

-type bitcoin_addr_message() :: #bitcoin_addr_message {}.

-record(bitcoin_headers_message, {
    headers :: [bitcoin_block_header()]
}).

-type bitcoin_headers_message() :: #bitcoin_headers_message {}.

-record(bitcoin_getblocks_message, {
    version :: uint32_t(),
    block_locator_hashes :: [binary()],
    hash_stop :: binary()
}).

-type bitcoin_getblocks_message() :: #bitcoin_getblocks_message {}.

-record(bitcoin_getheaders_message, {
    version :: uint32_t(),
    block_locator_hashes :: [binary()],
    hash_stop :: binary()
}).

-type bitcoin_getheaders_message() :: #bitcoin_getheaders_message {}.

-record(bitcoin_tx_message, {
    version :: uint32_t(),
    transaction_inputs :: [bitcoin_transaction_input()],
    transaction_outputs :: [bitcoin_transaction_output()],
    lock_time :: uint32_t()
}).

-type bitcoin_tx_message() :: #bitcoin_tx_message {}.

-record(bitcoin_block_message, {
    version :: uint32_t(),
    previous_block :: binary(),
    merkle_root :: binary(),
    timestamp :: uint32_t(),
    bits :: uint32_t(),
    nonce :: uint32_t(),
    transactions :: [bitcoin_tx_message()]
}).

-type bitcoin_block_message() :: #bitcoin_block_message {}.

-record(bitcoin_message, {
    header :: bitcoin_message_header(),
    body :: bitcoin_verack_message()
          | bitcoin_ping_message()
          | bitcoin_getaddr_message()
          | bitcoin_version_message()
          | bitcoin_alert_message()
          | bitcoin_inv_message()
          | bitcoin_getdata_message()
          | bitcoin_notfound_message()
          | bitcoin_addr_message()
          | bitcoin_headers_message()
          | bitcoin_getblocks_message()
          | bitcoin_getheaders_message()
          | bitcoin_tx_message()
          | bitcoin_block_message()
}).

-type bitcoin_message() :: #bitcoin_message {}.

-record(block_index_entry, {
    hash :: binary(),
    height :: non_neg_integer(),
    previous = undefined :: undefined | binary(),
    next = undefined :: undefined | binary()
}).

-type block_index_entry() :: #block_index_entry {}.

-type block_locator() :: [binary()].
