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
%%% @doc Peculium's Address Manager.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_address_manager).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/0, remember/2, get_address/1]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types.
-type network() :: peculium_core_types:network().
-type peer_identifier() :: {inet:ip_address(), inet:port_number()}.
-type unix_epoch() :: peculium_core_types:unix_epoch().

-record(state, {}).

-record(address, {
    ip :: inet:ip_address(),
    network :: network()
}).

-define(SERVER, ?MODULE).

%% @doc Start the address management server.
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Remember a given address.
-spec remember(Address :: inet:ip_address(), Network :: network()) -> ok.
remember(Address, Network) ->
    gen_server:cast(?SERVER, {remember, Address, Network}).

%% @doc Get address.
-spec get_address(Network :: network()) -> {Address :: inet:ip_address()}.
get_address(Network) ->
    gen_server:call(?SERVER, {get_address, Network}).

%% @private
init([]) ->
    lager:info("Starting Address Management Server"),
    Options = [
        {disc_copies, [node()]},
        {attributes, record_info(fields, address)}
    ],
    case mnesia:create_table(address, Options) of
        {atomic, ok} ->
            {ok, #state {}};
        {aborted, {already_exists, address}} ->
            {ok, #state {}};
        {aborted, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call({get_address, Network}, _From, State) ->
    Reply = random_address(),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({remember, Address, Network}, State) ->
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write(#address{ ip = Address, network = Network })
    end),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Stopping Address Management Server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
-spec random_address() -> {Address :: inet:ip_address(), Network :: network()}.
random_address() ->
    Keys = mnesia:dirty_all_keys(address),
    Key = lists:nth(random:uniform(length(Keys)), Keys),
    [#address{ ip = Address }] = mnesia:dirty_read({address, Key}),
    Address.
