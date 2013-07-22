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
%%% @doc Cryptography Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_crypto).

%% API.
-export([hash/1]).

%% Types.
-type hash() :: peculium_core_types:hash().

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Returns the double SHA256 checksum of a given input.
-spec hash(Data :: iolist()) -> hash().
hash(Data) ->
    peculium_core_utilities:reverse(crypto:sha256(crypto:sha256(Data))).

-ifdef(TEST).

-spec hash_test() -> any().
hash_test() ->
    ?assertEqual(hash(<<>>), peculium_core_utilities:hex2bin("56944c5d3f98413ef45cf54545538103cc9f298e0575820ad3591376e2e0f65d")),
    ?assertEqual(hash(<<"Peculium Rocks!">>), peculium_core_utilities:hex2bin("9edea1a358e2bdadad97ed081c11c46c934e2cdacc248d350f168874b6273c8a")).

-spec prop_hash_pure() -> any().
prop_hash_pure() ->
    ?FORALL(X, binary(),
        hash(X) =:= hash(X)).

-spec prop_hash_iolist() -> any().
prop_hash_iolist() ->
    ?FORALL({A, B, C}, {binary(), binary(), binary()},
        hash([A, B, C]) =:= hash(<<A/binary, B/binary, C/binary>>)).

-endif.
