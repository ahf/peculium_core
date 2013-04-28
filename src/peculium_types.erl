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
%%%
%%% ----------------------------------------------------------------------------
%%% @author     Alexander Færøy <ahf@0x90.dk>
%%% @copyright  2013 Fearless Hamster Solutions
%%% @end
%%% ----------------------------------------------------------------------------
%%% @doc Peculium Types.
%%% This module contains common types used in the Peculium code.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_types).

%% Types.
-export_type([unit_atom/0, uint8_t/0, uint16_t/0, uint32_t/0,
        uint64_t/0, int8_t/0, int16_t/0, int32_t/0, int64_t/0,
        network_atom/0, command_atom/0, inv_atom/0,
        inv_integer/0, inv/0, checksum/0,
        transaction_outpoint/0, transaction_input/0,
        transaction_output/0, network_address/0,
        block_header/0, transaction/0, block/0,
        message_header/0, verack_message/0,
        ping_message/0, getaddr_message/0,
        version_message/0, alert_message/0,
        inv_message/0, bitcoin_getdata_message/0,
        bitcoin_notfound_message/0, bitcoin_addr_message/0,
        bitcoin_headers_message/0, bitcoin_getblocks_message/0,
        bitcoin_getheaders_message/0, bitcoin_tx_message/0,
        block_message/0, bitcoin_message/0, block_index_entry/0,
        block_locator/0]).

-include_lib("peculium/include/peculium.hrl").

-type unit_atom() :: megabitcoin | kilobitcoin | hectobitcoin | decabitcoin
                   | bitcoin | decibitcoin | centibitcoin | millibitcoin
                   | microbitcoin | satoshi.

-type uint8_t()  :: 0 .. 16#ff.
-type uint16_t() :: 0 .. 16#ffff.
-type uint32_t() :: 0 .. 16#ffffffff.
-type uint64_t() :: 0 .. 16#ffffffffffffffff.

-type int8_t()  :: -16#80 .. 16#7f.
-type int16_t() :: -16#8000 .. 16#7fff.
-type int32_t() :: -16#80000000 .. 16#7fffffff.
-type int64_t() :: -16#8000000000000000 .. 16#7fffffffffffffff.

-type checksum() :: <<_:32>>.

-type block_locator() :: [checksum()].

-type network_atom() :: mainnet | testnet | testnet3.

-type command_atom() :: addr | alert | block | checkorder
                              | getaddr | getblocks | getdata | getheaders
                              | headers | inv | ping | submitorder
                              | reply | tx | verack | version.

-type inv_atom() :: error | tx | block.
-type inv_integer() :: 0 | 1 | 2.

-opaque inv() :: #inv {}.

-opaque transaction_outpoint() :: #transaction_outpoint {}.

-opaque transaction_input() :: #transaction_input {}.

-opaque transaction_output() :: #transaction_output {}.

-opaque network_address() :: #network_address {}.

-opaque block_header() :: #block_header {}.

-opaque transaction() :: #transaction {}.

-opaque block() :: #block {}.

-opaque message_header() :: #message_header {}.

-opaque verack_message() :: #verack_message {}.

-opaque ping_message() :: #ping_message {}.

-opaque getaddr_message() :: #getaddr_message {}.

-opaque version_message() :: #version_message {}.

-opaque alert_message() :: #alert_message {}.

-opaque inv_message() :: #inv_message {}.

-opaque bitcoin_getdata_message() :: #bitcoin_getdata_message {}.

-opaque bitcoin_notfound_message() :: #bitcoin_notfound_message {}.

-opaque bitcoin_addr_message() :: #bitcoin_addr_message {}.

-opaque bitcoin_headers_message() :: #bitcoin_headers_message {}.

-opaque bitcoin_getblocks_message() :: #bitcoin_getblocks_message {}.

-opaque bitcoin_getheaders_message() :: #bitcoin_getheaders_message {}.

-opaque bitcoin_tx_message() :: #bitcoin_tx_message {}.

-opaque block_message() :: #block_message {}.

-opaque bitcoin_message() :: #bitcoin_message {}.

-opaque block_index_entry() :: #block_index_entry {}.
