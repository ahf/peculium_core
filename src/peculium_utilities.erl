-module(peculium_utilities).

-export([strip/2, timestamp/0]).
-export([hex2bin/1, bin2hex/1]).
-export([reverse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec reverse(binary()) -> binary().
reverse(Binary) when is_binary(Binary) ->
    Size = byte_size(Binary) * 8,
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.

-spec strip(binary(), binary()) -> binary().
strip(Subject, Pattern) ->
    Result = binary:split(Subject, [Pattern], [global, trim]),
    iolist_to_binary(Result).

-spec timestamp() -> non_neg_integer().
timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = now(),
    MegaSeconds * 1000000 + Seconds.

-spec hex2bin(string()) -> binary().
hex2bin([A, B | Rest]) ->
    <<(list_to_integer([A, B], 16)), (hex2bin(Rest))/binary>>;
hex2bin([A]) ->
    <<(list_to_integer([A], 16))>>;
hex2bin([]) ->
    <<>>.

-spec bin2hex(binary()) -> string().
bin2hex(Bin) when is_binary(Bin) ->
    lists:flatten([integer_to_list(X, 16) || <<X:4/integer>> <= Bin]).

-ifdef(TEST).

-spec test() -> any().

-spec strip_test() -> any().
strip_test() ->
    ?assertEqual(strip(<<1,2,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>),
    ?assertEqual(strip(<<0,0,0,1,0,2,0,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>).

-endif.
