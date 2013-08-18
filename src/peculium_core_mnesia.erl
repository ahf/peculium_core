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
%%% @doc Peculium Mnesia Utilities.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_mnesia).

%% API.
-export([init/0]).

%% @doc Initialize mnesia.
-spec init() -> ok | {error, term()}.
init() ->
    %% Overwrite Mnesia directory. This must be called after we have started
    %% the peculium_core_config process.
    application:set_env(mnesia, dir, peculium_core_config:mnesia_dir()),

    %% Ensure that our data directory is available.
    ok = ensure_mnesia_dir(),

    %% Stop Mnesia.
    ok = stop_mnesia(),

    %% Create schema.
    ok = create_schema(),

    %% Start Mnesia again.
    ok = start_mnesia().

%% @private
-spec create_schema() -> ok.
create_schema() ->
    case mnesia:system_info(use_dir) of
        true ->
            ok;
        false ->
            mnesia:create_schema([node()])
    end.

%% @private
-spec start_mnesia() -> ok.
start_mnesia() ->
    ok = mnesia:start(),
    ensure_mnesia_running().

%% @private
-spec stop_mnesia() -> ok.
stop_mnesia() ->
    stopped = mnesia:stop(),
    ensure_mnesia_stopped().

%% @private
-spec ensure_mnesia_dir() -> ok.
ensure_mnesia_dir() ->
    Directory = peculium_core_config:mnesia_dir(),
    case filelib:ensure_dir(Directory) of
        {error, Reason} ->
            throw({error, {unable_to_create_mnesia_dir, Directory, Reason}});
        ok ->
            ok
    end.

%% @private
-spec ensure_mnesia_running() -> ok.
ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            lager:debug("Waiting for Mnesia to start"),
            timer:sleep(timer:seconds(1)),
            ensure_mnesia_running();
        _Otherwise ->
            throw({error, mnesia_not_running})
    end.

%% @private
-spec ensure_mnesia_stopped() -> ok.
ensure_mnesia_stopped() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        stopping ->
            lager:debug("Waiting for Mnesia to stop"),
            timer:sleep(timer:seconds(1)),
            ensure_mnesia_stopped();
        _Otherwise ->
            throw({error, mnesia_not_stopping})
    end.
