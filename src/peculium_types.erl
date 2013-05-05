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
-export_type([unit_type/0, uint8_t/0, uint16_t/0, uint32_t/0, uint64_t/0,
        int8_t/0, int16_t/0, int32_t/0, int64_t/0, network/0, command/0,
        inv_type/0, script_op/0, script/0, inv_integer/0, inv/0, checksum/0,
        hash/0, transaction_outpoint/0, transaction_input/0,
        transaction_output/0, network_address/0, block_header/0, transaction/0,
        block/0, message_header/0, verack_message/0, ping_message/0,
        getaddr_message/0, version_message/0, alert_message/0, inv_message/0,
        getdata_message/0, notfound_message/0, addr_message/0,
        headers_message/0, getblocks_message/0, getheaders_message/0,
        transaction_message/0, block_message/0, message/0, block_index_entry/0,
        block_locator/0, message_types/0]).

-include_lib("peculium/include/peculium.hrl").

-type unit_type() :: megabitcoin | kilobitcoin | hectobitcoin | decabitcoin
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

-type hash() :: <<_:256>>.

-type block_locator() :: [hash()].

-type network() :: mainnet | testnet | testnet3.

-type command() :: addr | alert | block | checkorder
                 | getaddr | getblocks | getdata | getheaders
                 | headers | inv | ping | submitorder
                 | reply | transaction | verack | version.

-type inv_type() :: error | transaction | block.
-type inv_integer() :: 0 | 1 | 2.

-type script_op() :: op_0 | op_1negate | op_1 | op_2 | op_3 | op_4 | op_5
                   | op_6 | op_7 | op_8 | op_9 | op_10 | op_11 | op_11 | op_12
                   | op_13 | op_14 | op_15 | op_16 | op_nop | op_if | op_notif
                   | op_else | op_endif | op_verify | op_return | op_toaltstack
                   | op_fromaltstack | op_ifdup | op_depth | op_drop | op_dup
                   | op_nip | op_over | op_pick | op_roll | op_rot | op_swap
                   | op_tuck | op_2drop | op_2dup | op_3dup | op_2over | op_2rot
                   | op_2swap | op_cat | op_substr | op_left | op_right | op_size
                   | op_invert | op_and | op_xor | op_or | op_xor | op_equal
                   | op_equalverify | op_1add | op_1sub | op_2mul | op_2div
                   | op_negate | op_abs | op_not | op_0notequal | op_add | op_sub
                   | op_mul | op_div | op_mod | op_lshift | op_rshift | op_booland
                   | op_boolor | op_numequal | op_numequalverify | op_numnotequal
                   | op_lessthan | op_greaterthan | op_lessthanorequal
                   | op_greaterthanorequal | op_min | op_max | op_within | op_ripemd160
                   | op_sha1 | op_sha256 | op_hash160 | op_hash256 | op_codeseparator
                   | op_checksig | op_checksigverify | op_checkmultisig
                   | op_checkmultisigverify | op_ver.

-type script() :: [script_op() | binary()].

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

-opaque getdata_message() :: #getdata_message {}.

-opaque notfound_message() :: #notfound_message {}.

-opaque addr_message() :: #addr_message {}.

-opaque headers_message() :: #headers_message {}.

-opaque getblocks_message() :: #getblocks_message {}.

-opaque getheaders_message() :: #getheaders_message {}.

-opaque transaction_message() :: #transaction_message {}.

-opaque block_message() :: #block_message {}.

-opaque message() :: #message {}.

-opaque block_index_entry() :: #block_index_entry {}.

-type message_types() :: verack_message()
                       | ping_message()
                       | getaddr_message()
                       | version_message()
                       | alert_message()
                       | inv_message()
                       | getdata_message()
                       | notfound_message()
                       | addr_message()
                       | headers_message()
                       | getblocks_message()
                       | getheaders_message()
                       | transaction_message()
                       | block_message().
