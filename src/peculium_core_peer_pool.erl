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
%%% @doc Peer Pool.
%%% This module contains a `supervisor' for supervising `peculium_core_peer`
%%% servers.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_peer_pool).

%% Behaviour.
-behaviour(supervisor).

%% API.
-export([start_link/0, spawn_peer/2, spawn_peer/4]).

%% Supervisor callbacks.
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).
-define(SERVER, ?MODULE).

%% From supervisor.
-type startlink_err() :: {already_started, pid()} | shutdown | term().
-type startlink_ret() :: {ok, pid()} | ignore | {error, startlink_err()}.

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec spawn_peer(Address :: inet:ip_address(), Port :: inet:port_number()) -> {ok, pid()} | {error, term()}.
spawn_peer(Address, Port) ->
    supervisor:start_child(?SERVER, [Address, Port]).

-spec spawn_peer(ListenerPid :: pid(), Socket :: inet:socket(), Transport :: term(), Options :: [term()]) -> {ok, pid()} | {error, term()}.
spawn_peer(ListenerPid, Socket, Transport, Options) ->
    supervisor:start_child(?SERVER, [ListenerPid, Socket, Transport, Options]).

-spec init([]) -> {ok, {{simple_one_for_one, non_neg_integer(), non_neg_integer()}, []}}.
init(_State) ->
    {ok, {{simple_one_for_one, 60, 60}, [
        ?CHILD(peculium_core_peer, worker)
    ]}}.
