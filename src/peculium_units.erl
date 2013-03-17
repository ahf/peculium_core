-module(peculium_units).

-export([factor/1, convert/3, stringify/1]).

-include_lib("peculium/include/peculium.hrl").

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

-spec stringify(bitcoin_unit_atom()) -> binary().
stringify(X) ->
    atom_to_binary(X, utf8).

-spec convert(number(), bitcoin_unit_atom(), bitcoin_unit_atom()) -> float().
convert(Input, InputType, OutputType) ->
    Input * 1 / factor(InputType) * factor(OutputType).
