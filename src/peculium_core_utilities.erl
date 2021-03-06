%%%
%%% Copyright (c) 2013 Alexander Færøy.
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
%%% @copyright  2013 Alexander Færøy
%%% @end
%%% ----------------------------------------------------------------------------
%%% @doc Peculium Utilities
%%% This module contains various utilities used in Peculium.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_utilities).

%% API.
-export([expand_homedir/1, find_last/2, strip/2, timestamp/0,
        hex2bin/1, bin2hex/1, repeat/2, reverse/1,
        parallel_map/2, format_ip_address/1, format_ip_port/2]).

-include("peculium_core_test.hrl").

%% @doc Returns the path with the homedir expanded.
-spec expand_homedir(Path :: string()) -> string().
expand_homedir(Path) ->
    binary_to_list(iolist_to_binary(re:replace(Path, "~", find_homedir()))).

%% @doc Returns the last element of a given list that matches the given predicate.
-spec find_last(Pred :: fun((X :: term()) -> boolean()), List :: [term()]) -> term() | not_found.
find_last(Pred, List) ->
    case lists:filter(Pred, List) of
        [] ->
            not_found;
        Result ->
            lists:last(Result)
    end.

%% @doc Reverse a given binary.
-spec reverse(Data :: binary()) -> binary().
reverse(Binary) when is_binary(Binary) ->
    Size = byte_size(Binary) * 8,
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.

%% @doc Strip the pattern, Pattern, from the given Subject.
-spec strip(Subject :: binary(), Pattern :: binary()) -> binary().
strip(Subject, Pattern) ->
    Result = binary:split(Subject, [Pattern], [global, trim]),
    iolist_to_binary(Result).

%% @doc Returns current UNIX epoch timestamp.
-spec timestamp() -> non_neg_integer().
timestamp() ->
    {MegaSeconds, Seconds, _MicroSeconds} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.

%% @doc Convert hex to binary.
-spec hex2bin(Data :: string()) -> binary();
             (Data :: binary()) -> binary().
hex2bin([A, B | Rest]) ->
    <<(list_to_integer([A, B], 16)), (hex2bin(Rest))/binary>>;
hex2bin([A]) ->
    <<(list_to_integer([A], 16))>>;
hex2bin([]) ->
    <<>>;
hex2bin(X) when is_binary(X) ->
    hex2bin(binary_to_list(X)).

%% @doc Convert binary data to hex.
-spec bin2hex(Data :: binary()) -> binary().
bin2hex(Bin) when is_binary(Bin) ->
    list_to_binary(lists:flatten([integer_to_list(X, 16) || <<X:4/integer>> <= Bin])).

%% @doc Repeat a function N times and return the list of return values.
-spec repeat(N :: non_neg_integer(), Fun :: fun (() -> term())) -> [term()].
repeat(N, Fun) ->
    [Fun() || _ <- lists:seq(1, N)].

%% @doc Format a given IP address This function takes into
%% account that the IP address might be an IPv6 mapped IPv4
%% address and format it like an IPv4 address.
-spec format_ip_address(Address :: inet:ip_address()) -> string().
format_ip_address(Address) ->
    case Address of
        % IPv6 mapped IPv4 address.
        {0, 0, 0, 0, 0, 16#ffff, A, B} ->
            inet_parse:ntoa({A bsr 8, A - ((A bsr 8) bsl 8), B bsr 8, B - ((B bsr 8) bsl 8)});

        % IPv6 or IPv4 address.
        _Otherwise ->
            inet_parse:ntoa(Address)
    end.

%% @doc Format a given IP address and port.
-spec format_ip_port(Address :: inet:ip_address(), Port :: inet:port_number()) -> string().
format_ip_port(Address, Port) ->
    case Address of
        {_, _, _, _} ->
            io_lib:format("~s:~b", [format_ip_address(Address), Port]);

        {0, 0, 0, 0, 0, 16#ffff, _, _} ->
            io_lib:format("~s:~b", [format_ip_address(Address), Port]);

        {_, _, _, _, _, _, _, _} ->
            io_lib:format("[~s]:~b", [format_ip_address(Address), Port])
    end.

%% @doc Applies the function, Fun, to each element of List in parallel and
%% returns the result. The result shares the same order as the input list.
-spec parallel_map(Fun :: fun((X :: any()) -> any()), List :: [term()]) -> [term()].
parallel_map(Fun, List) ->
    Self = self(),
    Pids = [spawn_link(fun() ->
                parallel_map_fun(Self, Fun, X)
            end) || X <- List],
    parallel_map_gather_results(Pids).

%% @private
parallel_map_fun(Pid, Fun, X) ->
    Pid ! {self(), Fun(X)}.

%% @private
parallel_map_gather_results([Pid | Rest]) ->
    receive
        {Pid, Value} -> [Value | parallel_map_gather_results(Rest)]
    end;
parallel_map_gather_results([]) ->
    [].

%% @private
-spec find_homedir() -> string().
find_homedir() ->
    case os:getenv("HOME") of
        false ->
            [];
        Homedir ->
            Homedir
    end.

-ifdef(TEST).

-spec hex2bin_test() -> any().
hex2bin_test() ->
    [
        ?assertEqual(hex2bin("FFffFF"), <<255,255,255>>),
        ?assertEqual(hex2bin("0"), <<0>>),
        ?assertEqual(hex2bin(""), <<>>)
    ].

-spec hex2bin_single_test() -> any().
hex2bin_single_test() ->
    ?assertEqual(hex2bin(<<"f">>), <<15>>).

-spec bin2hex_test() -> any().
bin2hex_test() ->
    [
        ?assertEqual(bin2hex(<<>>), <<>>),
        ?assertEqual(bin2hex(<<255,255,255>>), <<"FFFFFF">>),
        ?assertEqual(bin2hex(<<15>>), <<"0F">>)
    ].

-spec strip_test() -> any().
strip_test() ->
    ?assertEqual(strip(<<1,2,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>),
    ?assertEqual(strip(<<0,0,0,1,0,2,0,3,4,0,0,0>>, <<0>>), <<1,2,3,4>>).

-spec parallel_map_ordering_test() -> any().
parallel_map_ordering_test() ->
    ?assertEqual(parallel_map(fun (X) -> X end, [a, b, c, d, e]), [a, b, c, d, e]).

-spec reverse_test() -> any().
reverse_test() ->
    [
        ?assertEqual(reverse(<<>>), <<>>),
        ?assertEqual(reverse(<<"abc">>), <<"cba">>)
    ].

-spec find_last_test() -> any().
find_last_test() ->
    Fun = fun ({A, _}) -> A =:= 9 end,
    [
        ?assertEqual(find_last(Fun, [{1, no}, {2, no}, {5, no}, {8, no}, {9, no}, {9, yes}, {10, no}]), {9, yes}),
        ?assertEqual(find_last(Fun, [{1, no}, {2, no}, {5, no}, {8, no}, {10, no}]), not_found)
    ].

-spec timestamp_test() -> any().
timestamp_test() ->
    A = timestamp(),
    timer:sleep(timer:seconds(1)),
    B = timestamp(),
    ?assert(A < B).

-spec prop_hex2bin_bin2hex_inverse() -> any().
prop_hex2bin_bin2hex_inverse() ->
    ?FORALL(X, binary(),
        hex2bin(bin2hex(X)) == X).

-spec prop_repeat_length() -> any().
prop_repeat_length() ->
    ?FORALL(X, non_neg_integer(),
        length(repeat(X, fun () -> test end)) =:= X).

-endif.
