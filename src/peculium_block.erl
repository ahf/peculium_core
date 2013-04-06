%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Utilities.
-module(peculium_block).

%% API.
-export([hash/1, genesis_block/1, transactions/1]).

-include_lib("peculium/include/peculium.hrl").
-include_lib("erl_aliases/include/erl_aliases.hrl").

-module_alias({t, peculium_bitcoin_protocol_types}).

%% @doc Returns the little-endian encoded hash of a given block.
-spec hash(bitcoin_block_message()) -> binary().
hash(#bitcoin_block_message { version = Version, previous_block = PreviousBlock, merkle_root = MerkleRoot, timestamp = Timestamp, bits = Bits, nonce = Nonce }) ->
    peculium_utilities:reverse(crypto:sha256(crypto:sha256([t:uint32_t(Version), PreviousBlock, MerkleRoot, t:uint32_t(Timestamp), t:uint32_t(Bits), t:uint32_t(Nonce)]))).

%% @doc Returns the Genesis block from a given network.
-spec genesis_block(bitcoin_network_atom()) -> bitcoin_block_message().
genesis_block(mainnet) ->
    Inputs = [#bitcoin_transaction_input {
        sequence = 16#ffffffff,
        previous_output = #bitcoin_outpoint {
            index = 16#ffffffff,
            hash = <<0:256>>
        },
        script = <<4,255,255,0,29,1,4,69,84,104,101,32,84,105,109,101,115,32,48,51,47,74,97,110,47,
                   50,48,48,57,32,67,104,97,110,99,101,108,108,111,114,32,111,110,32,98,114,105,110,
                   107,32,111,102,32,115,101,99,111,110,100,32,98,97,105,108,111,117,116,32,102,111,
                   114,32,98,97,110,107,115>>
    }],
    Outputs = [#bitcoin_transaction_output {
        value = 5000000000,
        script = <<65,4,103,138,253,176,254,85,72,39,25,103,241,166,113,48,183,16,92,214,168,40,224,
                   57,9,166,121,98,224,234,31,97,222,182,73,246,188,63,76,239,56,196,243,85,4,229,30,
                   193,18,222,92,56,77,247,186,11,141,87,138,76,112,43,107,241,29,95,172>>
    }],
    #bitcoin_block_message {
        version = 1,
        previous_block = <<0:256>>,
        merkle_root = <<59,163,237,253,122,123,18,178,122,199,44,62,103,118,143,97,127,200,27,195,136,138,81,50,58,159,184,170,75,30,94,74>>,
        timestamp = 1231006505,
        bits = 16#1d00ffff,
        nonce = 2083236893,
        transactions = [#bitcoin_tx_message {
            version = 1,
            transaction_inputs = Inputs,
            transaction_outputs = Outputs,
            lock_time = 0
        }]
    }.

%% @doc Returns a list of transactions from a given block.
transactions(#bitcoin_block_message { transactions = Transactions }) ->
    Transactions.
