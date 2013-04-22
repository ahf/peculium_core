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
%%% @doc        Bitcoin Transaction Utilities.
%%% @reference  <a href="https://en.bitcoin.it/wiki/Transactions">Transactions</a>
%%%             <em>The Bitcoin community</em>.
%%% ----------------------------------------------------------------------------
-module(peculium_transaction).

%% API.
-export([hash/1, inputs/1, outputs/1, version/1, lock_time/1, is_coinbase/1]).

-include_lib("peculium/include/peculium.hrl").
-include_lib("erl_aliases/include/erl_aliases.hrl").

-module_alias({t, peculium_protocol_types}).

%% @doc Returns the hash of a given transaction.
-spec hash(bitcoin_transaction()) -> binary().
hash(#bitcoin_transaction { version = Version, transaction_inputs = Inputs, transaction_outputs = Outputs, lock_time = LockTime }) ->
    {ok, InputsLength} = t:var_int(length(Inputs)),
    InputsBin = lists:map(fun peculium_protocol_types:transaction_input/1, Inputs),
    {ok, OutputsLength} = t:var_int(length(Outputs)),
    OutputsBin = lists:map(fun peculium_protocol_types:transaction_output/1, Outputs),
    Data = [t:uint32_t(Version), InputsLength, InputsBin, OutputsLength, OutputsBin, t:uint32_t(LockTime)],
    peculium_crypto:hash(Data).

%% @doc Returns the transaction inputs of a given transaction.
-spec inputs(bitcoin_transaction()) -> [bitcoin_transaction_input()].
inputs(#bitcoin_transaction { transaction_inputs = TransactionInputs }) ->
    TransactionInputs.

%% @doc Returns the transaction outputs of a given transaction.
-spec outputs(bitcoin_transaction()) -> [bitcoin_transaction_output()].
outputs(#bitcoin_transaction { transaction_outputs = TransactionOutputs }) ->
    TransactionOutputs.

%% @doc Returns the version of a given transaction.
-spec version(bitcoin_transaction()) -> uint32_t().
version(#bitcoin_transaction { version = Version }) ->
    Version.

%% @doc Returns the lock time of a given transaction.
-spec lock_time(bitcoin_transaction()) -> uint32_t().
lock_time(#bitcoin_transaction { lock_time = LockTime }) ->
    LockTime.

%% @doc Check if a given transaction is a coinbase transaction.
-spec is_coinbase(Transaction :: bitcoin_transaction()) -> boolean().
is_coinbase(Transaction) ->
    Inputs = inputs(Transaction),
    length(Inputs) =:= 1 andalso peculium_outpoint:hash(peculium_transaction_input:previous_output(hd(Inputs))) =:= <<0:256>>.
