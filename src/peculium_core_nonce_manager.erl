%%%
%%% copyright (c) 2013 Alexander Færøy.
%%% all rights reserved.
%%%
%%% redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% * redistributions of source code must retain the above copyright notice, this
%%%   list of conditions and the following disclaimer.
%%%
%%% * redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% this software is provided by the copyright holders and contributors "as is" and
%%% any express or implied warranties, including, but not limited to, the implied
%%% warranties of merchantability and fitness for a particular purpose are
%%% disclaimed. in no event shall the copyright holder or contributors be liable
%%% for any direct, indirect, incidental, special, exemplary, or consequential
%%% damages (including, but not limited to, procurement of substitute goods or
%%% services; loss of use, data, or profits; or business interruption) however
%%% caused and on any theory of liability, whether in contract, strict liability,
%%% or tort (including negligence or otherwise) arising in any way out of the use
%%% of this software, even if advised of the possibility of such damage.
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
-module(peculium_core_nonce_manager).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/0, create/0, has/1, delete/1]).

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
-spec has(Nonce :: binary()) -> boolean().
has(Nonce) ->
    gen_server:call(?SERVER, {has, Nonce}).

%% @doc Create a new nonce.
-spec create() -> binary().
create() ->
    gen_server:call(?SERVER, create).

%% @doc Delete a given nonce.
-spec delete(Nonce :: binary()) -> ok.
delete(Nonce) ->
    gen_server:cast(?SERVER, {delete, Nonce}).

%% @private
init([]) ->
    lager:info("Starting Nonce Management Server"),
    {ok, #state {
        nonces = sets:new()
    } }.

%% @private
handle_call(create, _From, #state { nonces = Nonces } = State) ->
    Nonce = crypto:rand_bytes(8),
    {reply, Nonce, State#state { nonces = sets:add_element(Nonce, Nonces) }};

handle_call({has, Nonce}, _From, #state { nonces = Nonces } = State) ->
    {reply, sets:is_element(Nonce, Nonces), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({delete, Nonce}, #state { nonces = Nonces } = State) ->
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
