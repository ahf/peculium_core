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
%%% @doc Peculium Nonce Manager
%%% This module contains a `gen_server' for managing connection nonces.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_peer_nonce_manager).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/0, create_nonce/0, has_nonce/1, remove_nonce/1]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.

-record(state, {
    nonces :: set()
}).

%% Tests.
-include("peculium_core_test.hrl").

-define(SERVER, ?MODULE).

%% @doc Start the nonce management server.
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if a given nonce is used by one of our active connections.
-spec has_nonce(Nonce :: binary()) -> boolean().
has_nonce(Nonce) ->
    gen_server:call(?SERVER, {has_nonce, Nonce}).

%% @doc Create a new nonce.
-spec create_nonce() -> binary().
create_nonce() ->
    gen_server:call(?SERVER, create_nonce).

%% @doc Delete a given nonce.
-spec remove_nonce(Nonce :: binary()) -> ok.
remove_nonce(Nonce) ->
    gen_server:cast(?SERVER, {remove_nonce, Nonce}).

%% @private
init([]) ->
    lager:info("Starting Nonce Management Server"),
    {ok, #state {
        nonces = sets:new()
    } }.

%% @private
handle_call(create_nonce, _From, #state { nonces = Nonces } = State) ->
    Nonce = crypto:rand_bytes(8),
    {reply, Nonce, State#state { nonces = sets:add_element(Nonce, Nonces) }};

handle_call({has_nonce, Nonce}, _From, #state { nonces = Nonces } = State) ->
    {reply, sets:is_element(Nonce, Nonces), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({remove_nonce, Nonce}, #state { nonces = Nonces } = State) ->
    {noreply, State#state { nonces = sets:del_element(Nonce, Nonces) }};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Stopping Nonce Management Server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
