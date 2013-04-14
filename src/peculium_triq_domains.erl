%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Type domains for Triq.
-module(peculium_triq_domains).

%% API.
-export([bitcoin_unit_atom/0]).

-include_lib("peculium/include/peculium.hrl").

-spec bitcoin_unit_atom() -> triq_dom:domain(any()).
bitcoin_unit_atom() ->
    triq_dom:elements([satoshi, microbitcoin, millibitcoin, centibitcoin, decibitcoin, bitcoin, decabitcoin, hectobitcoin, kilobitcoin, megabitcoin]).
