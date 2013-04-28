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
%%% @copyright  2013 Fearless Hamster Solutions
%%% @end
%%% ----------------------------------------------------------------------------
%%% @doc Bitcoin Unit Utilities.
%%% This module contains utilities to help converting to and from various
%%% Bitcoin unit types.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_units).

%% API.
-export([factor/1, convert/3, stringify/1]).

%% Types.
-type bitcoin_unit_atom() :: peculium_types:bitcoin_unit_atom().

-include_lib("peculium/include/peculium.hrl").

%% @doc Returns the factor of a given unit.
-spec factor(Unit :: bitcoin_unit_atom()) -> float().
factor(Unit) ->
    case Unit of
        megabitcoin ->
            1000000.0;
        kilobitcoin ->
            1000.0;
        hectobitcoin ->
            100.0;
        decabitcoin ->
            10.0;
        bitcoin ->
            1.0;
        decibitcoin ->
            0.1;
        centibitcoin ->
            0.01;
        millibitcoin ->
            0.001;
        microbitcoin ->
            0.000001;
        satoshi ->
            0.00000001
    end.

%% @doc Convert a unit to a binary.
-spec stringify(Unit :: bitcoin_unit_atom()) -> binary().
stringify(Unit) ->
    atom_to_binary(Unit, utf8).

%% @doc Convert a given number from the input unit to the output unit.
-spec convert(Value :: float(), InputUnit :: bitcoin_unit_atom(), OutputUnit :: bitcoin_unit_atom()) -> float().
convert(Value, InputUnit, OutputUnit) ->
    Value * 1 / factor(InputUnit) * factor(OutputUnit).

-ifdef(TEST).

-spec compare(float(), float()) -> boolean().
compare(A, B) ->
    %% FIXME: Horror ahead.
    As = lists:flatten(io_lib:format("~.12f", [A])),
    Bs = lists:flatten(io_lib:format("~.12f", [B])),
    As =:= Bs.

prop_convert_integer_inverse() ->
    ?FORALL({Value, InputUnit, OutputUnit}, {pos_integer(), peculium_triq_domains:bitcoin_unit_atom(), peculium_triq_domains:bitcoin_unit_atom()},
        compare(convert(convert(Value, InputUnit, OutputUnit), OutputUnit, InputUnit), float(Value))).

prop_convert_real_inverse() ->
    ?FORALL({Value, InputUnit, OutputUnit}, {real(), peculium_triq_domains:bitcoin_unit_atom(), peculium_triq_domains:bitcoin_unit_atom()},
        compare(convert(convert(Value, InputUnit, OutputUnit), OutputUnit, InputUnit), float(Value))).

-endif.
