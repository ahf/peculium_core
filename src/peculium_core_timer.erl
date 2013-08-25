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
%%% @doc Process Timer.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_timer).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/4, stop/1, reset/1]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    module :: atom(),
    function :: atom(),
    arguments :: [term()],
    interval :: non_neg_integer(),
    timer :: reference()
}).

-define(SERVER, ?MODULE).

%% @doc Start timer process.
-spec start_link(Interval :: non_neg_integer(), Module :: atom(), Function :: atom(), Arguments :: [term()]) -> {ok, pid()} | {error, any()}.
start_link(Interval, Module, Function, Arguments) ->
    gen_server:start_link(?SERVER, [Interval, Module, Function, Arguments], []).

%% @doc Stop the timer process.
-spec stop(Timer :: pid()) -> ok.
stop(Timer) ->
    gen_server:cast(Timer, stop).

%% @doc Reset the timer process.
-spec reset(Timer :: pid()) -> ok.
reset(Timer) ->
    gen_server:cast(Timer, reset).

%% @private
init([Interval, Module, Function, Arguments]) ->
    {ok, start_timer(#state {
        module = Module,
        function = Function,
        arguments = Arguments,
        interval = Interval
    })}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(reset, State) ->
    {noreply, reset_timer(State)};

handle_cast(stop, State) ->
    {stop, normal, stop_timer(State)};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(tick, #state { module = Module, function = Function, arguments = Arguments } = State) ->
    erlang:apply(Module, Function, Arguments),
    {noreply, reset_timer(State)};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
-spec start_timer(State :: term()) -> NewState :: term().
start_timer(#state { interval = Interval } = State) ->
    State#state { timer = erlang:send_after(Interval, self(), tick) }.

%% @private
-spec stop_timer(State :: term()) -> NewState :: term().
stop_timer(#state { timer = Timer } = State) ->
    State#state { timer = erlang:cancel_timer(Timer) }.

%% @private
-spec reset_timer(State :: term()) -> NewState :: term().
reset_timer(State) ->
    start_timer(stop_timer(State)).
