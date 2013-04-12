-module(peculium_utilities).

-export([strip/2, timestamp/0]).
-export([hex2bin/1, bin2hex/1]).
-export([reverse/1]).
-export([parallel_map/2]).

-include_lib("peculium/include/peculium.hrl").

-include("peculium_test.hrl").

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

-spec hex2bin(string()) -> binary();
             (binary()) -> binary().
hex2bin([A, B | Rest]) ->
    <<(list_to_integer([A, B], 16)), (hex2bin(Rest))/binary>>;
hex2bin([A]) ->
    <<(list_to_integer([A], 16))>>;
hex2bin([]) ->
    <<>>;
hex2bin(X) when is_binary(X) ->
    hex2bin(binary_to_list(X)).

-spec bin2hex(binary()) -> binary().
bin2hex(Bin) when is_binary(Bin) ->
    list_to_binary(lists:flatten([integer_to_list(X, 16) || <<X:4/integer>> <= Bin])).

parallel_map(Fun, List) ->
    Self = self(),
    Pids = [spawn_link(fun() ->
                parallel_map_fun(Self, Fun, X)
            end) || X <- List],
    parallel_map_gather_results(Pids).

parallel_map_fun(Pid, Fun, X) ->
    Pid ! {self(), Fun(X)}.

parallel_map_gather_results([Pid | Rest]) ->
    receive
        {Pid, Value} -> [Value | parallel_map_gather_results(Rest)]
    end;
parallel_map_gather_results([]) ->
    [].

-ifdef(TEST).

-spec strip_test() -> any().
strip_test() ->
    ?assertEqual(strip(<<1,2,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>),
    ?assertEqual(strip(<<0,0,0,1,0,2,0,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>).

prop_inverse() ->
    ?FORALL(X, binary(),
        hex2bin(bin2hex(X)) == X).

-endif.
