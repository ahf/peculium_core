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
%%% @doc        Bitcoin Transaction Input Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_transaction_input).

%% API.
-export([previous_output/1, script/1, sequence/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the previous output of a given transaction input.
-spec previous_output(TransactionInput :: bitcoin_transaction_input()) -> bitcoin_transaction_outpoint().
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