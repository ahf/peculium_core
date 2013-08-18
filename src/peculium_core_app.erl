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
%%% @doc        The Peculium Application.
%%% ----------------------------------------------------------------------------
-module(peculium_core_app).
-behaviour(application).

%% API.
-export([start/0, start/2, stop/1]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(peculium_core).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_, _) ->
    %% Initialize Mnesia.
    ok = peculium_core_mnesia:init(),

    %% Start the supervisor.
    Result = peculium_core_sup:start_link(),

    %% FIXME: Test code.
    %% Insert Genesis block into the index.
    peculium_core_block_index:insert(peculium_core_block:genesis_block(mainnet)),

    %% Given a Bitcoin network, which port should we listen to?
    {ok, Port} = peculium_core_network:port_number(peculium_core_config:network()),

    %% Start listener.
    {ok, _} = ranch:start_listener(peculium_core_peer, 100,
        ranch_tcp, [
            {port, Port},
            {max_connections, peculium_core_config:max_peers()}
        ], peculium_core_peer_pool, []),

    %% Return result from the supervisor.
    Result.

-spec stop([]) -> ok.
stop(_State) ->
    ok.
