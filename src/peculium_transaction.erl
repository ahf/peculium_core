%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Utilities.
%% @reference <a href="https://en.bitcoin.it/wiki/Transactions">Transactions</a>
%%            <em>The Bitcoin community</em>.
-module(peculium_transaction).

%% API.
-export([hash/1, inputs/1, outputs/1, version/1, lock_time/1, is_coinbase/1]).

-include_lib("peculium/include/peculium.hrl").
-include_lib("erl_aliases/include/erl_aliases.hrl").

-module_alias({t, peculium_protocol_types}).

%% @doc Returns the hash of a given transaction.
-spec hash(bitcoin_tx_message()) -> binary().
hash(#bitcoin_tx_message { version = Version, transaction_inputs = Inputs, transaction_outputs = Outputs, lock_time = LockTime }) ->
    {ok, InputsLength} = t:var_int(length(Inputs)),
    InputsBin = lists:map(fun peculium_protocol_types:transaction_input/1, Inputs),
    {ok, OutputsLength} = t:var_int(length(Outputs)),
    OutputsBin = lists:map(fun peculium_protocol_types:transaction_output/1, Outputs),
    Data = [t:uint32_t(Version), InputsLength, InputsBin, OutputsLength, OutputsBin, t:uint32_t(LockTime)],
    peculium_crypto:hash(Data).

%% @doc Returns the transaction inputs of a given transaction.
-spec inputs(bitcoin_tx_message()) -> [bitcoin_transaction_input()].
inputs(#bitcoin_tx_message { transaction_inputs = TransactionInputs }) ->
    TransactionInputs.

%% @doc Returns the transaction outputs of a given transaction.
-spec outputs(bitcoin_tx_message()) -> [bitcoin_transaction_output()].
outputs(#bitcoin_tx_message { transaction_outputs = TransactionOutputs }) ->
    TransactionOutputs.

%% @doc Returns the version of a given transaction.
-spec version(bitcoin_tx_message()) -> uint32_t().
version(#bitcoin_tx_message { version = Version }) ->
    Version.

%% @doc Returns the lock time of a given transaction.
-spec lock_time(bitcoin_tx_message()) -> uint32_t().
lock_time(#bitcoin_tx_message { lock_time = LockTime }) ->
    LockTime.

%% @doc Check if a given transaction is a coinbase transaction.
-spec is_coinbase(Transaction :: bitcoin_tx_message()) -> boolean().
is_coinbase(Transaction) ->
    Inputs = inputs(Transaction),
    length(Inputs) =:= 1 andalso peculium_outpoint:hash(peculium_transaction_input:previous_output(hd(Inputs))) =:= <<0:256>>.
