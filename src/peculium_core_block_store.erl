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
%%% @doc Bitcoin Block Store Server.
%%% This module contains a `gen_server' server for storing and retrieving
%%% Bitcoin block's.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_block_store).

%% API.
-export([start_link/0, exists/1, put/2, delete/1, get/1]).

%% Types.
-type block() :: peculium_core_types:block().
-type hash() :: peculium_core_types:hash().

-include_lib("peculium_core/include/peculium_core.hrl").

%% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    db :: eleveldb:db_ref()
}).

%% @doc Check if a given block exists in the store.
-spec exists(Hash :: hash()) -> boolean().
exists(Hash) ->
    gen_server:call(?SERVER, {exists, Hash}).

%% @doc Insert block.
-spec put(Hash :: hash(), Block :: block()) -> ok.
put(Hash, Block)->
    gen_server:call(?SERVER, {put, Hash, Block}, infinity).

%% @doc Delete block.
-spec delete(Hash :: hash()) -> ok.
delete(Hash) ->
    gen_server:cast(?SERVER, {delete, Hash}).

%% @doc Get block.
-spec get(Hash :: hash()) -> {ok, block()} | {error, not_found}.
get(Hash) ->
    gen_server:call(?SERVER, {get, Hash}).

%% @doc Start Block Store Server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
    lager:info("Starting Block Store Server"),
    ok = filelib:ensure_dir(peculium_core_config:block_chain_dir() ++ "/"),
    {ok, Db} = peculium_core_leveldb:open(peculium_core_config:block_store_dir()),
    {ok, #state {
        db = Db
    }}.

%% @private
handle_call({exists, Hash}, _from, #state { db = Db } = State) ->
    case peculium_core_leveldb:get(Db, Hash) of
        {ok, _} ->
            {reply, true, State};
        not_found ->
            {reply, false, State}
    end;
handle_call({put, Hash, Block}, _From, #state { db = Db } = State) ->
    lager:info("Adding ~s to block store", [binary_to_list(peculium_core_utilities:bin2hex(Hash))]),
    peculium_core_leveldb:put(Db, Hash, iolist_to_binary(peculium_core_protocol_types:block(Block))),
    {reply, ok, State};
handle_call({get, Hash}, _from, #state { db = Db } = State) ->
    case peculium_core_leveldb:get(Db, Hash) of
        {ok, Block} ->
            {reply, peculium_core_protocol_types:block(Block), State};
        not_found ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({delete, Hash}, #state { db = Db } = State) ->
    peculium_core_leveldb:delete(Db, Hash),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Stopping Block Store Server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
