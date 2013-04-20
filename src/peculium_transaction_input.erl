%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Input Utilities.
-module(peculium_transaction_input).

%% API.
-export([previous_output/1, script/1, sequence/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the previous output of a given transaction input.
-spec previous_output(TransactionInput :: bitcoin_transaction_input()) -> bitcoin_outpoint().
previous_output(#bitcoin_transaction_input { previous_output = PreviousOutput }) ->
    PreviousOutput.

%% @doc Returns the script of a given transaction input.
-spec script(TransactionInput :: bitcoin_transaction_input()) -> binary().
script(#bitcoin_transaction_input { script = Script }) ->
    Script.

%% @doc Returns the sequence of a given transaction input.
-spec sequence(TransactionInput :: bitcoin_transaction_input()) -> uint32_t().
sequence(#bitcoin_transaction_input { sequence = Sequence }) ->
    Sequence.
