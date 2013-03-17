-module(peculium_utilities).

-export([strip/2, timestamp/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec strip(binary(), binary()) -> binary().
strip(Subject, Pattern) ->
    Result = binary:split(Subject, [Pattern], [global, trim]),
    iolist_to_binary(Result).

-spec timestamp() -> non_neg_integer().
timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = now(),
    MegaSeconds * 1000000 + Seconds.

-ifdef(TEST).

-spec test() -> any().

-spec strip_test() -> any().
strip_test() ->
    ?assertEqual(strip(<<1,2,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>),
    ?assertEqual(strip(<<0,0,0,1,0,2,0,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>).

-endif.
