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
-export([start_link/0, stop/1, connect/3, ping/1, verack/1, getaddr/1,
        version/1, getdata/2, getblocks/3, getheaders/3, block/8]).

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

-record(state, {
    listener :: undefined | pid(),
    socket :: undefined | inet:socket(),
    continuation :: binary(),
    inbound :: boolean(),
    sent = 0 :: non_neg_integer(),
    received = 0 :: non_neg_integer(),
    received_version :: undefined | version_message(),
    network = mainnet :: network()
}).

-type state() :: #state {}.
-type peer() :: pid().

%% Tests.
-include("peculium_core_test.hrl").

%% @private
-spec test_connect() -> peer().
test_connect() ->
    test_connect({127, 0, 0, 1}).

%% @private
-spec test_connect(Address :: inet:ip_address()) -> peer().
test_connect(Address) ->
    {ok, Peer} = start_link(),
    connect(Peer, Address, 8333),
    Peer.

-define(SERVER, ?MODULE).

%% @doc Start Peer server.
-spec start_link() -> {ok, peer()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @private
%% Used by Ranch to start listener server.
start_link(ListenerPid, Socket, _Transport, Options) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Options], []).

%% @doc Stop the given Peer server.
-spec stop(Peer :: peer()) -> ok.
stop(Peer) ->
    gen_server:cast(Peer, stop).

connect(Peer, Address, Port) ->
    gen_server:cast(Peer, {connect, Address, Port}).

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
-spec version(Peer :: peer()) -> ok.
version(Peer) ->
    %% Note: The arguments will be added by the Peer.
    send_message(Peer, version).

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

-spec init(Arguments :: [any()]) -> {ok, state()} | {ok, state(), non_neg_integer() | infinity} | {ok, state(), hibernate} | {stop, any()} | ignore.
init([]) ->
    {ok, #state {
        listener = undefined,
        socket = undefined,
        continuation = <<>>,
        inbound = false,
        received_version = undefined
    } };

init([ListenerPid, Socket, _Options]) ->
    %% Note: The timeout.
    %% See handle_info(timeout, ...) for more information.
    {ok, #state {
        listener = ListenerPid,
        socket = Socket,
        continuation = <<>>,
        inbound = true,
        received_version = undefined
    }, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, stopped, State};

handle_cast({connect, Address, Port}, State) ->
    case gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, once}]) of
        {ok, Socket} ->
            version(self()),
            {noreply, State#state { socket = Socket }};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_cast({message, version, Arguments}, #state { network = Network, socket = Socket } = State) ->
    {ok, {SourceAddress, SourcePort}} = inet:sockname(Socket),
    {ok, {DestinationAddress, DestinationPort}} = inet:peername(Socket),
    {noreply, send(State, version, [Network, SourceAddress, SourcePort, DestinationAddress, DestinationPort])};

handle_cast({message, Message, Arguments}, #state { network = Network } = State) ->
    {noreply, send(State, Message, [Network | Arguments])};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(timeout, #state { listener = ListenerPid, socket = Socket } = State) ->
    ok = ranch:accept_ack(ListenerPid),
    ack_socket(Socket),
    {noreply, State};

handle_info({tcp, Socket, Packet}, #state { socket = Socket } = State) ->
    handle_transport_packet(State, Packet);

handle_info({tcp_closed, Socket}, #state { socket = Socket } = State) ->
    {stop, closed, State};

handle_info({tcp_error, Socket, Reason}, #state { socket = Socket } = State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

ack_socket(Socket) ->
    inet:setopts(Socket, [{active, once}]).

handle_transport_packet(#state { socket = Socket, continuation = Cont, received = Received } = State, Packet) ->
    ack_socket(Socket),
    case process_stream_chunk(Cont, Packet) of
        {ok, NewCont} ->
            {noreply, State#state { continuation = NewCont, received = byte_size(Packet) + Received }};
        {messages, Messages, NewCont} ->
            process_messages(State#state { continuation = NewCont }, Messages)
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
            {messages, lists:reverse(Messages), Data}
    end.

process_messages(#state { network = Network } = State, [#message { header = #message_header { network = MessageNetwork, length = Length, valid = Valid }, body = Body } = Message | Messages]) ->
    log(State, "Received ~p on ~p (~b bytes)", [element(1, Body), Network, Length]),
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

process_one_message(State, #message { body = #inv_message { inventory = Invs } }) ->
%%    LastBlockInv = peculium_core_utilities:find_last(fun peculium_core_inv:is_block/1, Invs),
    getdata(self(), peculium_core_inv:unknown_invs(Invs)),
    getblocks(self(), peculium_core_block_locator:from_best_block(), <<0:256>>),
    State;
%%    lists:foldl(fun (Inv, StateCont) ->
%%            StateCont2 = case Inv of
%%                LastBlockInv ->
%%                    send(StateCont, getblocks, [Network, peculium_core_block_locator:from_best_block(), <<0:256>>]);
%%                _Otherwise ->
%%                    StateCont
%%            end,
%%            send(StateCont2, getdata, [Network, [Inv]])
%%        end, State, Invs);

process_one_message(State, #message { body = #block_message { block = Block } }) ->
    %% peculium_core_block_index:insert(Block),
    State;

process_one_message(State, #message { body = #version_message {} = Version }) ->
    verack(self()),
    getaddr(self()),
    State#state { received_version = Version };

process_one_message(State, #message { body = #verack_message {} }) ->
    getblocks(self(), peculium_core_block_locator:from_best_block(), <<0:256>>),
    State;

process_one_message(State, _) ->
    State.

%% @private
-spec send(State :: state(), Message :: command(), Arguments :: [any()]) -> state().
send(#state { socket = Socket, sent = Sent } = State, Message, Arguments) ->
    Packet = apply(peculium_core_messages, Message, Arguments),
    PacketLength = iolist_size(Packet),
    log(State, "Sending ~p (~b bytes)", [Message, PacketLength]),
    ok = gen_tcp:send(Socket, Packet),
    State#state { sent = Sent + PacketLength }.

%% @private
-spec log(State :: state(), Format :: string(), Arguments :: [any()]) -> ok.
log(State, Format, Arguments) ->
    {ok, {Address, Port}} = inet:peername(State#state.socket),
    lager:debug([{peer, Address, Port}], "[Peer ~s (~b)] -> " ++ Format, [inet_parse:ntoa(Address), Port] ++ Arguments).

%% @private
-spec send_message(Peer :: peer(), Message :: command()) -> ok.
send_message(Peer, Message) ->
    send_message(Peer, Message, []).

%% @private
-spec send_message(Peer :: peer(), Message :: command(), Arguments :: [any()]) -> ok.
send_message(Peer, Message, Arguments) ->
    gen_server:cast(Peer, {message, Message, Arguments}).
