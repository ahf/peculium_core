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
%%% @doc        Bitcoin Inv Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_inv).

%% API.
-export([type/1, hash/1, is_transaction/1, is_block/1, unknown_invs/1, known/1, unknown/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the type of a given inv.
-spec type(bitcoin_inv()) -> bitcoin_inv_atom().
type(#bitcoin_inv { type = Type }) ->
    Type.

%% @doc Returns the hash of a given inv.
-spec hash(bitcoin_inv()) -> binary().
hash(#bitcoin_inv { hash = Hash }) ->
    Hash.

%% @doc Checks if a given inv is a transaction.
-spec is_transaction(bitcoin_inv()) -> boolean().
is_transaction(Inv) ->
    type(Inv) =:= tx.

%% @doc Checks if a given inv is a block.
-spec is_block(bitcoin_inv()) -> boolean().
is_block(Inv) ->
    type(Inv) =:= block.

%% @doc Returns a list of inv objects that we do not currently have.
-spec unknown_invs(Invs :: [bitcoin_inv()]) -> [bitcoin_inv()].
unknown_invs(Invs) ->
    lists:filter(fun unknown/1, Invs).

%% @doc Check if we have the given object.
-spec known(Inv :: bitcoin_inv()) -> boolean().
known(#bitcoin_inv { type = Type, hash = Hash }) ->
    case Type of
        block ->
            peculium_block_index:exists(Hash);
        tx ->
            %% FIXME: Once we have a transaction database, this should be changed.
            false
    end.

%% @doc Check if we do not have the given object.
-spec unknown(Inv :: bitcoin_inv()) -> boolean().
unknown(Inv) ->
    not known(Inv).
