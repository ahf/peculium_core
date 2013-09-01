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
%%% @doc Peer Server Manager.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_peer_manager).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/0, register_peer/1, unregister_peer/1,
        register_connected_peer/1, peer_count/0, connected_peer_count/0,
        peers/0, connected_peers/0]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.
-type peer() :: pid().

-record(state, {
    peers :: set(),
    connected_peers :: set()
}).

-define(SERVER, ?MODULE).
-define(INTERVAL, 5).

%% Tests.
-include("peculium_core_test.hrl").

%% @doc Start the peer management server.
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register peer.
-spec register_peer(Peer :: peer()) -> ok.
register_peer(Peer) when is_pid(Peer) ->
    gen_server:cast(?SERVER, {register_peer, Peer}).

%% @doc Unregister peer.
-spec unregister_peer(Peer :: peer()) -> ok.
unregister_peer(Peer) when is_pid(Peer) ->
    gen_server:cast(?SERVER, {unregister_peer, Peer}).

%% @doc Register peer as being connected.
-spec register_connected_peer(Peer :: peer()) -> ok.
register_connected_peer(Peer) when is_pid(Peer) ->
    gen_server:cast(?SERVER, {register_connected_peer, Peer}).

%% @doc Get number of peers.
-spec peer_count() -> non_neg_integer().
peer_count() ->
    gen_server:call(?SERVER, peer_count).

%% @doc Get number of connected peers.
-spec connected_peer_count() -> non_neg_integer().
connected_peer_count() ->
    gen_server:call(?SERVER, connected_peer_count).

%% @doc Get peers.
-spec peers() -> Peers :: [peer()].
peers() ->
    gen_server:call(?SERVER, peers).

%% @doc Get connected peers.
-spec connected_peers() -> Peers :: [peer()].
connected_peers() ->
    gen_server:call(?SERVER, connected_peers).

%% @private
init([]) ->
    lager:info("Starting Peer Management Server"),
    schedule_trigger(5),
    {ok, #state {
        peers = sets:new(),
        connected_peers = sets:new()
    }}.

%% @private
handle_call(peer_count, _From, #state { peers = Peers } = State) ->
    Reply = sets:size(Peers),
    {reply, Reply, State};

handle_call(connected_peer_count, _From, #state { connected_peers = ConnectedPeers } = State) ->
    Reply = sets:size(ConnectedPeers),
    {reply, Reply, State};

handle_call(peers, _From, #state { peers = Peers } = State) ->
    Reply = sets:to_list(Peers),
    {reply, Reply, State};

handle_call(connected_peers, _From, #state { connected_peers = ConnectedPeers } = State) ->
    Reply = sets:to_list(ConnectedPeers),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({register_peer, Peer}, #state { peers = Peers } = State) ->
    {noreply, State#state { peers = sets:add_element(Peer, Peers) }};

handle_cast({unregister_peer, Peer}, #state { peers = Peers, connected_peers = ConnectedPeers } = State) ->
    NewPeers = sets:del_element(Peer, Peers),
    case sets:is_element(Peer, ConnectedPeers) of
        true ->
            {noreply, State#state { peers = NewPeers, connected_peers = sets:del_element(Peer, ConnectedPeers) }};
        false ->
            {noreply, State#state { peers = NewPeers }}
    end;

handle_cast({register_connected_peer, Peer}, #state { connected_peers = ConnectedPeers } = State) ->
    {noreply, State#state { connected_peers = sets:add_element(Peer, ConnectedPeers) }};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(check_peers, #state { peers = Peers } = State) ->
    spawn_peers(Peers),
    schedule_trigger(?INTERVAL),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Stopping Peer Management Server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
-spec schedule_trigger(Seconds :: non_neg_integer()) -> ok.
schedule_trigger(Seconds) ->
    erlang:send_after(timer:seconds(Seconds), self(), check_peers).

%% @private
-spec spawn_peers(Peers :: set()) -> ok.
spawn_peers(Peers) ->
    MaxPeers = peculium_core_config:max_peers(),
    case sets:size(Peers) of
        Count when Count >= MaxPeers ->
            ok;
        Count ->
            MissingPeerCount = MaxPeers - Count,
            PeerCount = peculium_core_math:ceil(MissingPeerCount / 4.0),
            peculium_core_utilities:repeat(PeerCount, fun () ->
                Address = peculium_core_address_manager:get_address(mainnet),
                peculium_core_peer_pool:spawn_peer(Address, 8333)
            end),
            ok
    end.
