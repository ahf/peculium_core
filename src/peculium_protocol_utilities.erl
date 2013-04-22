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
%%% @doc        Bitcoin Protocol Utilities.
%%% ----------------------------------------------------------------------------
-module(peculium_protocol_utilities).

-export([inv_to_atom/1, atom_to_inv/1]).
-export([command_to_atom/1]).
-export([checksum/1]).

-include_lib("peculium/include/peculium.hrl").

-spec checksum(iolist()) -> bitcoin_checksum().
checksum(X) ->
    binary_part(crypto:sha256(crypto:sha256(X)), {0, 4}).

-spec inv_to_atom(integer()) -> {ok, bitcoin_inv_atom()} | {error, {invalid_inv_integer, any()}}.
inv_to_atom(0) ->
    {ok, error};
inv_to_atom(1) ->
    {ok, tx};
inv_to_atom(2) ->
    {ok, block};
inv_to_atom(X) ->
    {error, {invalid_inv_integer, X}}.

-spec atom_to_inv(bitcoin_inv_atom()) -> {ok, bitcoin_inv_integer()} | {error, {invalid_inv_atom, any()}}.
atom_to_inv(error) ->
    {ok, 0};
atom_to_inv(tx) ->
    {ok, 1};
atom_to_inv(block) ->
    {ok, 2};
atom_to_inv(X) ->
    {error, {invalid_inv_atom, X}}.

-spec command_to_atom(binary()) -> {ok, bitcoin_command_atom()} | {error, {invalid_command_atom, any()}}.
command_to_atom(Command) ->
    case peculium_utilities:strip(Command, <<0>>) of
        <<"addr">> ->
            {ok, addr};
        <<"alert">> ->
            {ok, alert};
        <<"block">> ->
            {ok, block};
        <<"checkorder">> ->
            {ok, checkorder};
        <<"getaddr">> ->
            {ok, getaddr};
        <<"notfound">> ->
            {ok, notfound};
        <<"getblocks">> ->
            {ok, getblocks};
        <<"getdata">> ->
            {ok, getdata};
        <<"getheaders">> ->
            {ok, getheaders};
        <<"headers">> ->
            {ok, headers};
        <<"inv">> ->
            {ok, inv};
        <<"ping">> ->
            {ok, ping};
        <<"reply">> ->
            {ok, reply};
        <<"submitorder">> ->
            {ok, submitorder};
        <<"tx">> ->
            {ok, tx};
        <<"verack">> ->
            {ok, verack};
        <<"version">> ->
            {ok, version};
        Value ->
            {error, {invalid_command_atom, Value}}
    end.
