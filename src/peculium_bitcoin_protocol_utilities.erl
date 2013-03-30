-module(peculium_bitcoin_protocol_utilities).

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
            {error, {invalid_command, Value}}
    end.
