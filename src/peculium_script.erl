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
%%% @doc Bitcoin Script Decoder
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_script).

%% API.
-export([decode/1]).

%% Types.
-type script() :: peculium_types:script().

%% Tests.

-spec decode(Opcodes :: binary()) -> {ok, script()} | {error, any()}.
decode(Opcodes) ->
    decode(Opcodes, []).

%% @private
-spec decode(Opcodes :: binary(), Result :: script()) -> {ok, script()} | {error, any()}.
decode(Opcodes, Result) ->
    case Opcodes of
        %% Constants.
        <<0, X/binary>> ->
            decode(X, [op_0 | Result]);
        <<Length:8/integer, Value:Length/binary, X/binary>> when Length >= 1, Length =< 75 ->
            decode(X, [Value | Result]);
        <<76, Length:8/integer, Value:Length/binary, X/binary>> ->
            decode(X, [Value | Result]);
        <<77, Length:16/integer, Value:Length/binary, X/binary>> ->
            decode(X, [Value | Result]);
        <<78, Length:32/integer, Value:Length/binary, X/binary>> ->
            decode(X, [Value | Result]);
        <<79, X/binary>> ->
            decode(X, [op_1negate | Result]);
        <<81, X/binary>> ->
            decode(X, [op_1 | Result]);
        <<82, X/binary>> ->
            decode(X, [op_2 | Result]);
        <<83, X/binary>> ->
            decode(X, [op_3 | Result]);
        <<84, X/binary>> ->
            decode(X, [op_4 | Result]);
        <<85, X/binary>> ->
            decode(X, [op_5 | Result]);
        <<86, X/binary>> ->
            decode(X, [op_6 | Result]);
        <<87, X/binary>> ->
            decode(X, [op_7 | Result]);
        <<88, X/binary>> ->
            decode(X, [op_8 | Result]);
        <<89, X/binary>> ->
            decode(X, [op_9 | Result]);
        <<90, X/binary>> ->
            decode(X, [op_10 | Result]);
        <<91, X/binary>> ->
            decode(X, [op_11 | Result]);
        <<92, X/binary>> ->
            decode(X, [op_12 | Result]);
        <<93, X/binary>> ->
            decode(X, [op_13 | Result]);
        <<94, X/binary>> ->
            decode(X, [op_14 | Result]);
        <<95, X/binary>> ->
            decode(X, [op_15 | Result]);
        <<96, X/binary>> ->
            decode(X, [op_16 | Result]);

        %% Flow Control.
        <<97, X/binary>> ->
            decode(X, [op_nop | Result]);
        <<99, X/binary>> ->
            decode(X, [op_if | Result]);
        <<100, X/binary>> ->
            decode(X, [op_notif | Result]);
        <<103, X/binary>> ->
            decode(X, [op_else | Result]);
        <<104, X/binary>> ->
            decode(X, [op_endif | Result]);
        <<105, X/binary>> ->
            decode(X, [op_verify | Result]);
        <<106, X/binary>> ->
            decode(X, [op_return | Result]);

        %% Stack.
        <<107, X/binary>> ->
            decode(X, [op_toaltstack | Result]);
        <<108, X/binary>> ->
            decode(X, [op_fromaltstack | Result]);
        <<109, X/binary>> ->
            decode(X, [op_2drop | Result]);
        <<110, X/binary>> ->
            decode(X, [op_2dup | Result]);
        <<111, X/binary>> ->
            decode(X, [op_3dup | Result]);
        <<112, X/binary>> ->
            decode(X, [op_2over | Result]);
        <<113, X/binary>> ->
            decode(X, [op_2rot | Result]);
        <<114, X/binary>> ->
            decode(X, [op_2swap | Result]);
        <<115, X/binary>> ->
            decode(X, [op_ifdup | Result]);
        <<116, X/binary>> ->
            decode(X, [op_depth | Result]);
        <<117, X/binary>> ->
            decode(X, [op_drop | Result]);
        <<118, X/binary>> ->
            decode(X, [op_dup | Result]);
        <<119, X/binary>> ->
            decode(X, [op_nip | Result]);
        <<120, X/binary>> ->
            decode(X, [op_over | Result]);
        <<121, X/binary>> ->
            decode(X, [op_pick | Result]);
        <<122, X/binary>> ->
            decode(X, [op_roll | Result]);
        <<123, X/binary>> ->
            decode(X, [op_rot | Result]);
        <<124, X/binary>> ->
            decode(X, [op_swap | Result]);
        <<125, X/binary>> ->
            decode(X, [op_tuck | Result]);

        %% Splice.
        <<126, X/binary>> ->
            decode(X, [op_cat | Result]);
        <<127, X/binary>> ->
            decode(X, [op_substr | Result]);
        <<128, X/binary>> ->
            decode(X, [op_left | Result]);
        <<129, X/binary>> ->
            decode(X, [op_right | Result]);
        <<130, X/binary>> ->
            decode(X, [op_size | Result]);

        %% Bitwise Logic.
        <<131, X/binary>> ->
            decode(X, [op_invert | Result]);
        <<132, X/binary>> ->
            decode(X, [op_and | Result]);
        <<133, X/binary>> ->
            decode(X, [op_or | Result]);
        <<134, X/binary>> ->
            decode(X, [op_xor | Result]);
        <<135, X/binary>> ->
            decode(X, [op_equal | Result]);
        <<136, X/binary>> ->
            decode(X, [op_equalverify | Result]);

        %% Arithmetic.
        <<139, X/binary>> ->
            decode(X, [op_1add | Result]);
        <<140, X/binary>> ->
            decode(X, [op_1sub | Result]);
        <<141, X/binary>> ->
            decode(X, [op_2mul | Result]);
        <<142, X/binary>> ->
            decode(X, [op_2div | Result]);
        <<143, X/binary>> ->
            decode(X, [op_negate | Result]);
        <<144, X/binary>> ->
            decode(X, [op_abs | Result]);
        <<145, X/binary>> ->
            decode(X, [op_not | Result]);
        <<146, X/binary>> ->
            decode(X, [op_0notequal | Result]);
        <<147, X/binary>> ->
            decode(X, [op_add | Result]);
        <<148, X/binary>> ->
            decode(X, [op_sub | Result]);
        <<149, X/binary>> ->
            decode(X, [op_mul | Result]);
        <<150, X/binary>> ->
            decode(X, [op_div | Result]);
        <<151, X/binary>> ->
            decode(X, [op_mod | Result]);
        <<152, X/binary>> ->
            decode(X, [op_lshift | Result]);
        <<153, X/binary>> ->
            decode(X, [op_rshift | Result]);
        <<154, X/binary>> ->
            decode(X, [op_booland | Result]);
        <<155, X/binary>> ->
            decode(X, [op_boolor | Result]);
        <<156, X/binary>> ->
            decode(X, [op_numequal | Result]);
        <<157, X/binary>> ->
            decode(X, [op_numequalverify | Result]);
        <<158, X/binary>> ->
            decode(X, [op_numnotequal | Result]);
        <<159, X/binary>> ->
            decode(X, [op_lessthan | Result]);
        <<160, X/binary>> ->
            decode(X, [op_greaterthan | Result]);
        <<161, X/binary>> ->
            decode(X, [op_lessthanorequal | Result]);
        <<162, X/binary>> ->
            decode(X, [op_greaterthanorequal | Result]);
        <<163, X/binary>> ->
            decode(X, [op_min | Result]);
        <<164, X/binary>> ->
            decode(X, [op_max | Result]);
        <<165, X/binary>> ->
            decode(X, [op_within | Result]);

        %% Crypto.
        <<166, X/binary>> ->
            decode(X, [op_ripemd160 | Result]);
        <<167, X/binary>> ->
            decode(X, [op_sha1 | Result]);
        <<168, X/binary>> ->
            decode(X, [op_sha256 | Result]);
        <<169, X/binary>> ->
            decode(X, [op_hash160 | Result]);
        <<170, X/binary>> ->
            decode(X, [op_hash256 | Result]);
        <<171, X/binary>> ->
            decode(X, [op_codeseparator | Result]);
        <<172, X/binary>> ->
            decode(X, [op_checksig | Result]);
        <<173, X/binary>> ->
            decode(X, [op_checksigverify | Result]);
        <<174, X/binary>> ->
            decode(X, [op_checkmultisig | Result]);
        <<175, X/binary>> ->
            decode(X, [op_checkmultisigverify | Result]);

        %% Pseudo-words.
        <<253, X/binary>> ->
            decode(X, [op_pubkeyhash | Result]);
        <<254, X/binary>> ->
            decode(X, [op_pubkey | Result]);

        %% Reserved.
        <<80, X/binary>> ->
            decode(X, [op_reserved | Result]);
        <<98, X/binary>> ->
            decode(X, [op_ver | Result]);
        <<101, X/binary>> ->
            decode(X, [op_verif | Result]);
        <<102, X/binary>> ->
            decode(X, [op_vernotif | Result]);
        <<137, X/binary>> ->
            decode(X, [op_reserved1 | Result]);
        <<138, X/binary>> ->
            decode(X, [op_reserved2 | Result]);
        <<Value:8/integer, X/binary>> when Value >= 176, Value =< 185 ->
            decode(X, [op_nop | Result]);

        %% Misc.
        <<_:8/integer, X/binary>> ->
            decode(X, [op_invalidopcode | Result]);
        <<>> ->
            {ok, lists:reverse(Result)};
        <<X/binary>> ->
            {error, {invalid_script, X, Result}}
    end.
