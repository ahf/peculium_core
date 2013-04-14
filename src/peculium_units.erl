%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Utilities for converting between units.
-module(peculium_units).

%% API.
-export([factor/1, convert/3, stringify/1]).

-include_lib("peculium/include/peculium.hrl").

-include("peculium_test.hrl").

%% @doc Returns the factor of a given unit.
-spec factor(bitcoin_unit_atom()) -> float().
factor(X) ->
    case X of
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
-spec stringify(bitcoin_unit_atom()) -> binary().
stringify(X) ->
    atom_to_binary(X, utf8).

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
