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
%%% @doc Peer Server.
%%% This module contains a `gen_server' for representing a peer in the Bitcoin
%%% peer-to-peer network.
%%%
%%% We are using a single server to represent both incoming and outgoing
%%% peers.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_peer).

%% Behaviour.
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% Global records.
-include_lib("peculium_core/include/peculium_core.hrl").

%% API.
-export([start_link/2, stop/1, ping/1, verack/1, getaddr/1, version/2,
        getdata/2, getblocks/3, getheaders/3, block/8, check_timeout/1]).

%% Test API.
%% FIXME: Kill, with fire.
-export([test_connect/0, test_connect/1]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Ranch Callbacks.
-export([start_link/4]).

%% Types.
-type block_locator() :: peculium_core_types:block_locator().
-type command() :: peculium_core_types:command().
-type hash() :: peculium_core_types:hash().
-type inv() :: peculium_core_types:inv().
-type network() :: peculium_core_types:network().
-type transaction() :: peculium_core_types:transaction().
-type uint32_t() :: peculium_core_types:uint32_t().
-type version_message() :: peculium_core_types:version_message().
-type peer() :: pid().
-type peername() :: {Address :: inet:ip_address(), Port :: inet:port_number()}.

-record(state, {
    listener = undefined :: undefined | pid(),
    socket = undefined :: undefined | inet:socket(),
    continuation = <<>> :: binary(),
    inbound :: boolean(),
    received_version = undefined :: undefined | version_message(),
    network = mainnet :: network(),
    nonce :: binary(),
    peername = undefined :: undefined | peername(),
    ping_timer = undefined :: undefined | pid(),
    ping_timestamp :: calendar:datetime()
}).

-define(SERVER, ?MODULE).

%% Tests.
-include("peculium_core_test.hrl").

%% @private
-spec test_connect() -> peer().
test_connect() ->
    test_connect({127, 0, 0, 1}).

%% @private
-spec test_connect(Address :: inet:ip_address()) -> peer().
test_connect(Address) ->
    {ok, Peer} = peculium_core_peer_pool:spawn_peer(Address, 8333),
    Peer.

%% @doc Start Peer server.
-spec start_link(Address :: inet:ip_address(), Port :: inet:port_number()) -> {ok, peer()} | ignore | {error, any()}.
start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

%% @private
%% Used by Ranch to start listener server.
-spec start_link(ListenerPid :: pid(), Socket :: inet:socket(), Transport :: term(), Options :: term()) -> {ok, peer()} | ignore | {error, any()}.
start_link(ListenerPid, Socket, _Transport, Options) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Options], []).

%% @doc Stop the given Peer.
-spec stop(Peer :: peer()) -> ok.
stop(Peer) ->
    gen_server:cast(Peer, stop).

%% @doc Send ping message to the given Peer.
-spec ping(Peer :: peer()) -> ok.
ping(Peer) ->
    send_message(Peer, ping).

%% @doc Send verack message to the given Peer.
-spec verack(Peer :: peer()) -> ok.
verack(Peer) ->
    send_message(Peer, verack).

%% @doc Send getaddr message to the given Peer.
-spec getaddr(Peer :: peer()) -> ok.
getaddr(Peer) ->
    send_message(Peer, getaddr).

%% @doc Send version message to the given Peer.
-spec version(Peer :: peer(), Nonce :: binary()) -> ok.
version(Peer, Nonce) ->
    %% Note: The arguments will be added by the Peer.
    send_message(Peer, version, [Nonce]).

%% @doc Send getdata message to the given Peer.
-spec getdata(Peer :: peer(), Invs :: [inv()]) -> ok.
getdata(Peer, Invs) ->
    send_message(Peer, getdata, [Invs]).

%% @doc Send getblocks message to the given Peer.
-spec getblocks(Peer :: peer(), BlockLocator :: block_locator(), BlockStop :: hash()) -> ok.
getblocks(Peer, BlockLocator, BlockStop) ->
    send_message(Peer, getblocks, [BlockLocator, BlockStop]).

%% @doc Send getheaders message to the given Peer.
-spec getheaders(Peer :: peer(), BlockLocator :: block_locator(), BlockStop :: hash()) -> ok.
getheaders(Peer, BlockLocator, BlockStop) ->
    send_message(Peer, getheaders, [BlockLocator, BlockStop]).

%% @doc Send block message to the given Peer.
-spec block(Peer :: peer(), Version :: uint32_t(), PreviousBlock :: hash(), MerkleRoot :: hash(), Timestamp :: non_neg_integer(), Bits :: binary(), Nonce :: binary(), Transactions :: [transaction()]) -> ok.
block(Peer, Version, PreviousBlock, MerkleRoot, Timestamp, Bits, Nonce, Transactions) ->
    send_message(Peer, block, [Version, PreviousBlock, MerkleRoot, Timestamp, Bits, Nonce, Transactions]).

%% @doc Check if a given peer is inactive.
-spec check_timeout(Peer :: peer()) -> ok.
check_timeout(Peer) ->
    gen_server:cast(Peer, check_timeout).

-spec init(Arguments :: [term()]) -> {ok, term()} | {ok, term(), non_neg_integer() | infinity} | {ok, term(), hibernate} | {stop, any()} | ignore.
init([Address, Port]) ->
    connect(self(), Address, Port),
    peculium_core_peer_manager:register_peer(self()),
    {ok, start_ping_timer(#state {
        inbound = false,
        peername = {Address, Port},
        nonce = peculium_core_peer_nonce_manager:create_nonce(),
        ping_timestamp = erlang:localtime()
    })};

init([ListenerPid, Socket, _Options]) ->
    %% Note: The timeout.
    %% See handle_info(timeout, ...) for more information.
    peculium_core_peer_manager:register_peer(self()),
    {ok, Peername} = inet:peername(Socket),
    {ok, start_ping_timer(#state {
        listener = ListenerPid,
        socket = Socket,
        inbound = true,
        nonce = peculium_core_peer_nonce_manager:create_nonce(),
        peername = Peername,
        ping_timestamp = erlang:localtime()
    }), 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({connect, Address, Port}, #state { nonce = Nonce } = State) ->
    log(State, info, "Connecting to peer ..."),
    case gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, once}], 10000) of
        {ok, Socket} ->
            version(self(), Nonce),
            {noreply, State#state { socket = Socket }};
        {error, Reason} ->
            log(State, info, "Unable to connect to peer: ~p", [Reason]),
            {stop, normal, State}
    end;

handle_cast(check_timeout, #state { ping_timestamp = PingTimeout, received_version = ReceivedVersion } = State) ->
    TimeDelta = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - calendar:datetime_to_gregorian_seconds(PingTimeout),
    PeerTimeout = peculium_core_config:peer_timeout(),
    if
        TimeDelta >= PeerTimeout ->
            log(State, info, "Timeout: ~b seconds", [TimeDelta]),
            {stop, normal, State};

        ReceivedVersion =:= undefined ->
            % Do not ping unless we have received a version message already.
            {noreply, reset_ping_timer(State)};

        true ->
            ping(self()),
            {noreply, reset_ping_timer(State)}
    end;

handle_cast(stop, State) ->
    {stop, normal, stopped, State};

handle_cast({message, version, [Nonce]}, #state { network = Network, socket = Socket, peername = Peername } = State) ->
    %% FIXME: sockname should be the local network address and not the socket name.
    {ok, {SourceAddress, SourcePort}} = inet:sockname(Socket),
    {DestinationAddress, DestinationPort} = Peername,
    {noreply, send(State, version, [Network, SourceAddress, SourcePort, DestinationAddress, DestinationPort, Nonce])};

handle_cast({message, Message, Arguments}, #state { network = Network } = State) ->
    {noreply, send(State, Message, [Network | Arguments])};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, #state { listener = ListenerPid, socket = Socket, nonce = Nonce, peername = {Address, Port} } = State) ->
    log(State, info, "Incoming connection from: ~s:~b", [inet_parse:ntoa(Address), Port]),
    ok = ranch:accept_ack(ListenerPid),
    ack_socket(Socket),
    %% FIXME: We should only talk once someone has talked to us.
    version(self(), Nonce),
    {noreply, State};

handle_info({tcp, Socket, Packet}, #state { socket = Socket } = State) ->
    handle_transport_packet(State, Packet);

handle_info({tcp_closed, Socket}, #state { socket = Socket } = State) ->
    log(State, info, "Remote peer closed the connection"),
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #state { socket = Socket } = State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state { nonce = Nonce } = State) ->
    log(State, info, "Shutting down"),
    peculium_core_peer_nonce_manager:remove_nonce(Nonce),
    peculium_core_peer_manager:unregister_peer(self()),
    stop_ping_timer(State),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

-spec ack_socket(Socket :: inet:socket()) -> ok.
ack_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]).

handle_transport_packet(#state { socket = Socket, continuation = Cont } = State, Packet) ->
    ack_socket(Socket),
    NewState = reset_ping_timer(reset_ping_timestamp(State)),
    case process_stream_chunk(Cont, Packet) of
        {ok, NewCont} ->
            {noreply, NewState#state { continuation = NewCont }};
        {messages, Messages, NewCont} ->
            process_messages(NewState#state { continuation = NewCont }, Messages);
        {error, Reason} ->
            log(NewState, error, "Error: ~p", [Reason]),
            {stop, normal, NewState}
    end.

process_stream_chunk(Cont, Packet) ->
    process_stream_chunk(Cont, Packet, []).

process_stream_chunk(Cont, Packet, Messages) ->
    Data = <<Cont/binary, Packet/binary>>,
    case peculium_core_protocol:decode(Data) of
        {ok, Message, <<>>} ->
            {messages, lists:reverse([Message | Messages]), <<>>};
        {ok, Message, Rest} ->
            process_stream_chunk(<<>>, Rest, [Message | Messages]);
        {error, insufficient_data} ->
            {messages, lists:reverse(Messages), Data};
        {error, Reason, _} ->
            {error, Reason}
    end.

process_messages(#state { network = Network } = State, [#message { header = #message_header { network = MessageNetwork, length = Length, valid = Valid } } = Message | Messages]) ->
    {ok, Format} = peculium_core_message_formatters:format(Message),
    log(State, info, "Received ~s on ~p (~b bytes)", [Format, Network, Length]),
    NewState = case Valid andalso Network =:= MessageNetwork of
        true ->
            process_one_message(State, Message);
        false ->
            lager:warning("Ignoring invalid message: ~p", [Message]),
            State
    end,
    process_messages(NewState, Messages);

process_messages(State, []) ->
    {noreply, State}.

process_one_message(State, #message { body = #block_message { block = Block } }) ->
    peculium_core_block_index:insert(Block),
    State;

process_one_message(State, #message { body = #version_message { nonce = Nonce } = Version }) ->
    %% FIXME: Check if we have already received a version message.
    case peculium_core_peer_nonce_manager:has_nonce(Nonce) of
        true ->
            log(State, warning, "Attempt to connect to ourself was prevented"),
            {stop, normal, State};

        false ->
            verack(self()),
            getaddr(self()),
            State#state { received_version = Version }
    end;

process_one_message(#state { network = Network } = State, #message { body = #addr_message { addresses = Addresses }}) ->
    lists:foreach(fun (#network_address { address = Address }) -> peculium_core_address_manager:remember(Address, Network) end, Addresses),
    State;

process_one_message(State, #message { body = #inv_message { inventory = Invs }}) ->
    getdata(self(), Invs),
    State;

process_one_message(State, #message { body = #verack_message {} }) ->
    peculium_core_peer_manager:register_connected_peer(self()),
    State;

process_one_message(State, _) ->
    State.

%% @private
-spec send(State :: term(), Message :: command(), Arguments :: [any()]) -> term().
send(#state { socket = Socket } = State, Message, Arguments) ->
    Packet = apply(peculium_core_messages, Message, Arguments),
    PacketLength = iolist_size(Packet),
    log(State, info, "Sending ~p (~b bytes)", [Message, PacketLength]),
    case gen_tcp:send(Socket, Packet) of
        ok ->
            State;
        {error, Reason} ->
            log(State, error, "Error: ~p", [Reason]),
            {stop, normal, State}
    end.

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string()) -> ok.
log(State, LogLevel, Format) ->
    log(State, LogLevel, Format, []).

%% @private
-spec log(State :: term(), LogLevel :: atom(), Format :: string(), Arguments :: [any()]) -> ok.
log(#state { peername = Peername }, LogLevel, Format, Arguments) ->
    {Address, Port} = Peername,
    lager:log(LogLevel, [{peer, Address, Port}], "Peer(~s): " ++ Format, [peculium_core_utilities:format_ip_port(Address, Port) | Arguments]).

%% @private
-spec send_message(Peer :: peer(), Message :: command()) -> ok.
send_message(Peer, Message) ->
    send_message(Peer, Message, []).

%% @private
-spec send_message(Peer :: peer(), Message :: command(), Arguments :: [any()]) -> ok.
send_message(Peer, Message, Arguments) ->
    gen_server:cast(Peer, {message, Message, Arguments}).

%% @private
-spec connect(Peer :: peer(), Address :: inet:ip_address(), Port :: inet:port_number()) -> ok.
connect(Peer, Address, Port) ->
    gen_server:cast(Peer, {connect, Address, Port}).

%% @private
-spec start_ping_timer(State :: term()) -> NewState :: term().
start_ping_timer(State) ->
    case peculium_core_timer:start_link(timer:seconds(peculium_core_config:peer_timeout() div 2), peculium_core_peer, check_timeout, [self()]) of
        {ok, TimerPid} ->
            State#state { ping_timer = TimerPid };
        {error, Reason} ->
            log(State, error, "Unable to create timer: ~p", [Reason]),
            State
    end.

%% @private
-spec reset_ping_timer(State :: term()) -> NewState :: term().
reset_ping_timer(#state { ping_timer = PingTimer } = State) ->
    case PingTimer of
        PingTimer when is_pid(PingTimer) ->
            peculium_core_timer:reset(PingTimer);
        undefined ->
            ok
    end,
    State.

%% @private
-spec stop_ping_timer(State :: term()) -> ok.
stop_ping_timer(#state { ping_timer = PingTimer }) ->
    case PingTimer of
        PingTimer when is_pid(PingTimer) ->
            peculium_core_timer:stop(PingTimer);
        undefined ->
            ok
    end.

%% @private
-spec reset_ping_timestamp(State :: term()) -> NewState :: term().
reset_ping_timestamp(State) ->
    State#state { ping_timestamp = erlang:localtime() }.
