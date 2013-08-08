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
%%% @doc        Peculium's Configuration server.
%%% ----------------------------------------------------------------------------
-module(peculium_core_config).
-behaviour(gen_server).

%% API.
-export([start_link/0, data_dir/0, block_chain_dir/0, block_store_dir/0,
        block_index_dir/0, mnesia_dir/0, cache_size/0]).

%% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% @doc Get the configuration directory.
-spec data_dir() -> string().
data_dir() ->
    peculium_core_utilities:expand_homedir(call(data_dir)).

%% @doc Get the block chain directory.
-spec block_chain_dir() -> string().
block_chain_dir() ->
    filename:join([data_dir(), "blockchain"]).

%% @doc Get the block store directory.
-spec block_store_dir() -> string().
block_store_dir() ->
    filename:join([block_chain_dir(), "store"]).

%% @doc Get the block index directory.
-spec block_index_dir() -> string().
block_index_dir() ->
    filename:join([block_chain_dir(), "index"]).

%% @doc Get the Mnesia directory.
-spec mnesia_dir() -> string().
mnesia_dir() ->
    filename:join([data_dir(), "mnesia"]).

%% @doc Get the cache size in bytes.
-spec cache_size() -> non_neg_integer().
cache_size() ->
    call(cache_size).

%% @doc Start the configuration server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
    lager:info("Starting configuration server"),
    {ok, #state {}}.

%% @private
handle_call({value, Key}, _From, State) ->
    {reply, value(Key), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Terminating configuration server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
value(Key) ->
    case application:get_env(peculium_core, Key) of
        undefined ->
            not_found;
        {ok, Value} ->
            Value
    end.

%% @private
call(Key) ->
    case gen_server:call(?SERVER, {value, Key}) of
        not_found ->
            exit({missing_config_value, Key});
        Value ->
            Value
    end.
