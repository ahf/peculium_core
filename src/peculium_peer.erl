%%%
%%% Copyright (c) 2013 Fearless Hamster Solutions.
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
%%% @doc        Peculium's Peer Client server.
%%% ----------------------------------------------------------------------------
-module(peculium_peer).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include_lib("peculium/include/peculium.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/4]).
-export([connect/3, send_message/3, stop/1]).
-export([test_connect/0, test_connect/1]).

test_connect() ->
    test_connect({127,0,0,1}).

test_connect(Address) when is_tuple(Address) ->
    {ok, Peer} = start_link(),
    connect(Peer, Address, 8333),
    Peer;

test_connect(Address) when is_list(Address) ->
    {ok, Ip} = inet_parse:address(Address),
    test_connect(Ip).

-define(SERVER, ?MODULE).

-record(state, {
    listener :: pid(),
    socket :: inet:socket(),
    continuation :: binary(),
    inbound = false :: boolean(),
    sent = 0 :: non_neg_integer(),
    received = 0 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ListenerPid, Socket, _Transport, Options) ->
    gen_server:start_link(?MODULE, [ListenerPid, Socket, Options], []).

connect(Peer, Address, Port) ->
    gen_server:cast(Peer, {connect, Address, Port}).

send_message(Peer, Message, Arguments) ->
    gen_server:cast(Peer, {message, Message, Arguments}).

stop(Peer) ->
    gen_server:cast(Peer, stop).

init([]) ->
    {ok, #state {
        listener = undefined,
        socket = undefined,
        continuation = <<>>
    } };

init([ListenerPid, Socket, _Options]) ->
    %% Note: The timeout.
    {ok, #state {
        listener = ListenerPid,
        socket = Socket,
        continuation = <<>>,
        inbound = true
    }, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, stopped, State};
handle_cast({connect, Address, Port}, #state { socket = OldSocket } = State) ->
    case OldSocket of
        undefined ->
            case gen_tcp:connect(Address, Port, [binary, {packet, 0}, {active, once}]) of
                {ok, Socket} ->
                    State2 = send(State#state { socket = Socket }, version, [mainnet, {{127,0,0,1}, 8000}, {Address, Port}]),
                    {noreply, State2};
                {error, Reason} ->
                    {stop, Reason, State}
            end;
        _Otherwise ->
            {reply, {error, already_connected}, State}
    end;
handle_cast({message, Message, Arguments}, State) ->
    {noreply, send(State, Message, Arguments)};
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
handle_info({tcp_error, Socket, Reason}, #state { socket = Socket} = State) ->
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
    case peculium_protocol:decode(Data) of
        {ok, Message, <<>>} ->
            {messages, lists:reverse([Message | Messages]), <<>>};
        {ok, Message, Rest} ->
            process_stream_chunk(<<>>, Rest, [Message | Messages]);
        {error, insufficient_data} ->
            {messages, lists:reverse(Messages), Data}
    end.

process_messages(State, [#bitcoin_message { header = #bitcoin_message_header { network = Network, length = Length, valid = Valid }, body = Body } = Message | Messages]) ->
    log(State, "Received ~p on ~p (~b bytes)", [element(1, Body), Network, Length]),
    NewState = case Valid of
        true ->
            process_one_message(State, Message);
        false ->
            lager:debug("Ignoring invalid message: ~p", [Message]),
            State
    end,
    process_messages(NewState, Messages);
process_messages(State, []) ->
    {noreply, State}.

process_one_message(State, #bitcoin_message { header = #bitcoin_message_header { network = Network }, body = #bitcoin_inv_message { inventory = Invs } }) ->
%%    LastBlockInv = peculium_utilities:find_last(fun peculium_inv:is_block/1, Invs),
    State2 = send(State, getdata, [Network, peculium_inv:unknown_invs(Invs)]),
    State3 = send(State2, getblocks, [Network, peculium_block_locator:from_best_block(), <<0:256>>]),
    State3;
%%    lists:foldl(fun (Inv, StateCont) ->
%%            StateCont2 = case Inv of
%%                LastBlockInv ->
%%                    send(StateCont, getblocks, [Network, peculium_block_locator:from_best_block(), <<0:256>>]);
%%                _Otherwise ->
%%                    StateCont
%%            end,
%%            send(StateCont2, getdata, [Network, [Inv]])
%%        end, State, Invs);

process_one_message(State, #bitcoin_message { header = #bitcoin_message_header { network = Network }, body = #bitcoin_block_message { block = #bitcoin_block { previous_block = PreviousBlock, transactions = Transactions } = Block } }) ->
    peculium_block_index:insert(Block),
    State;

process_one_message(State, #bitcoin_message { header = #bitcoin_message_header { network = Network }, body = #bitcoin_version_message {} }) ->
    State2 = send(State, verack, [Network]),
    send(State2, getaddr, [Network]);

process_one_message(State, #bitcoin_message { header = #bitcoin_message_header { network = Network }, body = #bitcoin_verack_message {} }) ->
    send(State, getblocks, [Network, peculium_block_locator:from_best_block(), <<0:256>>]);

process_one_message(State, _) ->
    State.

send(#state { socket = Socket, sent = Sent } = State, Message, Arguments) ->
    Packet = peculium_messages:Message(Arguments),
    PacketLength = iolist_size(Packet),
    log(State, "Sending ~p (~b bytes)", [Message, PacketLength]),
    ok = gen_tcp:send(Socket, Packet),
    State#state { sent = Sent + PacketLength }.

log(State, Format, Arguments) ->
    {ok, {Address, Port}} = inet:peername(State#state.socket),
    lager:debug("[Peer ~s (~b)] -> " ++ Format, [inet_parse:ntoa(Address), Port] ++ Arguments).
