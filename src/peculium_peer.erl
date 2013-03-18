%% Copyright (c) 2013 Alexander Færøy
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(peculium_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/4]).
-export([connect/3]).

-define(SERVER, ?MODULE).

-record(state, {
    listener :: pid(),
    socket :: inet:socket(),
    continuation :: binary(),
    transport :: atom()
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ListenerPid, Socket, Transport, Options) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Transport, Options], []).

connect(Server, Address, Port) ->
    gen_server:call(Server, {connect, Address, Port}).

init([]) ->
    {ok, #state {
        listener = undefined,
        socket = undefined,
        transport = gen_tcp,
        continuation = <<>>
    } };

init([ListenerPid, Socket, Transport, _Options]) ->
    %% Note: The timeout.
    {ok, #state {
        listener = ListenerPid,
        socket = Socket,
        transport = Transport,
        continuation = <<>>
    }, 0}.

handle_call({connect, Address, Port}, _From, #state { transport = Transport } = State) ->
    case Transport:connect(Address, Port, [binary, {packet, 0}, {active, once}]) of
        {ok, Socket} ->
            {reply, ok, State#state { socket = Socket } };
        {error, Reason} ->
            {stop, Reason}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, #state { listener = ListenerPid, socket = Socket } = State) ->
    ok = ranch:accept_ack(ListenerPid),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp, Socket, Packet}, #state { socket = Socket } = State) ->
    lager:info("Packet: ~p", [Packet]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Socket}, #state { socket = Socket } = State) ->
    {stop, closed, State};
handle_info({tcp_error, Socket, Reason}, #state { socket = Socket} = State) ->
    {stop, Reason, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
