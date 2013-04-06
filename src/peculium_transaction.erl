%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Transaction Utilities.
%% @reference <a href="https://en.bitcoin.it/wiki/Transactions">Transactions</a>
%%            <em>The Bitcoin community</em>.
-module(peculium_transaction).

%% API.
-export([hash/1]).

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
    peculium_utilities:reverse(crypto:sha256(crypto:sha256([t:uint32_t(Version), InputsLength, InputsBin, OutputsLength, OutputsBin, t:uint32_t(LockTime)]))).
