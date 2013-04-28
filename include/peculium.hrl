%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions.
%%% All rights reserved.
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

-record(inv, {
    type :: peculium_types:inv_atom(),
    hash :: binary()
}).

-record(transaction_outpoint, {
    index :: peculium_types:uint32_t(),
    hash :: binary()
}).

-record(transaction_input, {
    previous_output :: peculium_types:transaction_outpoint(),
    script :: binary(),
    sequence :: peculium_types:uint32_t()
}).

-record(transaction_output, {
    value :: peculium_types:int64_t(),
    script :: binary()
}).

-record(network_address, {
    time :: peculium_types:uint32_t(),
    services :: peculium_types:uint64_t(),
    address :: inet:ip6_address(),
    port :: peculium_types:uint16_t()
}).

-record(block_header, {
    version :: peculium_types:uint32_t(),
    previous_block :: binary(),
    merkle_root :: binary(),
    timestamp :: peculium_types:uint32_t(),
    bits :: peculium_types:uint32_t(),
    nonce :: peculium_types:uint32_t(),
    transaction_count :: peculium_types:uint8_t()
}).

-record(transaction, {
    version :: peculium_types:uint32_t(),
    transaction_inputs :: [peculium_types:transaction_input()],
    transaction_outputs :: [peculium_types:transaction_output()],
    lock_time :: peculium_types:uint32_t()
}).

-record(block, {
    version :: peculium_types:uint32_t(),
    previous_block :: binary(),
    merkle_root :: binary(),
    timestamp :: peculium_types:uint32_t(),
    bits :: peculium_types:uint32_t(),
    nonce :: peculium_types:uint32_t(),
    transactions :: [peculium_types:transaction()]
}).

-record(message_header, {
    network :: peculium_types:network_atom(),
    command :: peculium_types:command_atom(),
    length :: peculium_types:uint32_t(),
    checksum :: peculium_types:checksum(),
    valid :: boolean()
}).

-record(verack_message, {}).

-record(ping_message, {}).

-record(getaddr_message, {}).

-record(version_message, {
    version :: peculium_types:int32_t(),
    services :: peculium_types:uint64_t(),
    timestamp :: peculium_types:int64_t(),
    to_address :: peculium_types:network_address(),
    from_address :: peculium_types:network_address(),
    user_agent :: binary(),
    start_height :: peculium_types:int32_t(),
    relay :: boolean(),
    nonce :: binary()
}).

-record(alert_message, {
    payload :: binary(),
    signature :: binary()
}).

-record(inv_message, {
    inventory :: [peculium_types:inv()]
}).

-record(bitcoin_getdata_message, {
    inventory :: [peculium_types:inv()]
}).

-record(bitcoin_notfound_message, {
    inventory :: [peculium_types:inv()]
}).

-record(bitcoin_addr_message, {
    addresses :: [peculium_types:network_address()]
}).

-record(bitcoin_headers_message, {
    headers :: [peculium_types:block_header()]
}).

-record(bitcoin_getblocks_message, {
    version :: peculium_types:uint32_t(),
    block_locator :: peculium_types:block_locator(),
    hash_stop :: binary()
}).

-record(bitcoin_getheaders_message, {
    version :: peculium_types:uint32_t(),
    block_locator :: peculium_types:block_locator(),
    hash_stop :: binary()
}).

-record(bitcoin_tx_message, {
    transaction :: peculium_types:transaction()
}).

-record(block_message, {
    block :: peculium_types:block()
}).

-record(bitcoin_message, {
    header :: peculium_types:message_header(),
    body :: peculium_types:verack_message()
          | peculium_types:ping_message()
          | peculium_types:getaddr_message()
          | peculium_types:version_message()
          | peculium_types:alert_message()
          | peculium_types:inv_message()
          | peculium_types:bitcoin_getdata_message()
          | peculium_types:bitcoin_notfound_message()
          | peculium_types:bitcoin_addr_message()
          | peculium_types:bitcoin_headers_message()
          | peculium_types:bitcoin_getblocks_message()
          | peculium_types:bitcoin_getheaders_message()
          | peculium_types:bitcoin_tx_message()
          | peculium_types:block_message()
}).

%% NOTE: We are going to keep one block_index_entry for every block in the
%% block chain in memory all the time.
%%
%% We should probably apply the following optimizations in the future:
%%
%%   - Use references for our hash, previous hash and next hash to reduce the
%%     runtime memory consumption with length(block chain) * 3 * 256-bit.
%%
%%     This is easily fixable, but remember to make sure that the on disk
%%     format must contain the actual 256-bit hashes. The block index cache
%%     should then do the mapping between a reference and our block hashes.
%%
%%     Remember: do not store references to disk. The Erlang reference counters
%%     are reset during node restarts.
%%
%%     If you are new to hacking on Peculium, this could potentially become an
%%     excellent first patch ;-)
%%
-record(block_index_entry, {
    %% The hash of our block.
    hash :: binary(),

    %% The hash of the previous block. This is always set except for the
    %% genesis block where this is set to undefined.
    previous = undefined :: undefined | binary(),

    %% The hash of the next block. This is always set unless the block is the
    %% current head of a fork or the main chain.
    next = undefined :: undefined | binary(),

    %% The height of our block.
    height :: undefined | non_neg_integer(),

    %% The block header.
    block_header :: peculium_types:block_header(),

    %% Number of transactions in this block.
    transaction_count :: non_neg_integer(),

    %% Total number of transactions in the chain including the transactions in
    %% this block.
    total_transaction_count :: undefined | non_neg_integer(),

    %% Total amount of work in the chain including the work in this block.
    total_chain_work :: undefined | non_neg_integer()
}).
