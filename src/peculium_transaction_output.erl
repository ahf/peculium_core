%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Output Utilities.
-module(peculium_transaction_output).

%% API.
-export([value/1, script/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the value of a given transaction output.
-spec value(TransactionOutput :: bitcoin_transaction_output()) -> int64_t().
value(#bitcoin_transaction_output { value = Value }) ->
    Value.

%% @doc Returns the script of a given transaction output.
-spec script(TransactionOutput :: bitcoin_transaction_output()) -> binary().
script(#bitcoin_transaction_output { script = Script }) ->
    Script.
