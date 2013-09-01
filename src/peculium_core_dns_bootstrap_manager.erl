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
%%% @doc DNS Bootstrap Manager.
%%% @end
%%% ----------------------------------------------------------------------------
-module(peculium_core_dns_bootstrap_manager).

%% Behaviour.
-behaviour(gen_server).

%% API.
-export([start_link/0, bootstrap/1]).

%% Gen_server Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-include_lib("kernel/include/inet.hrl").

-record(state, {}).

-define(SERVER, ?MODULE).

%% @doc Start the DNS bootstrap management server.
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Execute DNS bootstrap.
-spec bootstrap(Config :: proplists:proplist()) -> ok.
bootstrap(Config) ->
    gen_server:cast(?SERVER, {bootstrap, Config}).

%% @private
init([]) ->
    lager:info("Starting DNS Bootstrap Management Server"),
    {ok, #state {}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({bootstrap, Config}, State) ->
    case proplists:get_value(servers, Config) of
        undefined ->
            lager:warning("No DNS servers specified");
        Servers when is_list(Servers) ->
            do_lookups(Servers)
    end,
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Stopping DNS Bootstrap Management Server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
do_lookups(Servers) ->
    lists:flatten(peculium_core_utilities:parallel_map(fun do_one_lookup/1, flatten_servers(Servers))).

%% @Private
do_one_lookup({ipv4, Hostname}) ->
    do_one_ipv4_host(Hostname);
do_one_lookup({ipv6, Hostname}) ->
    do_one_ipv6_host(Hostname);
do_one_lookup({Method, Hostname}) ->
    lager:warning("Unknown lookup method: ~p for ~s", [Method, Hostname]),
    [].

%% @private
flatten_servers(Servers) ->
    flatten_servers(Servers, []).

%% @private
flatten_servers([{Hostname, Config} | Rest], Result) ->
    Hosts = lists:map(fun (Entry) ->
        case Entry of
            v4 ->
                {ipv4, Hostname};
            v6 ->
                {ipv6, Hostname};
            _ ->
                lager:warning("Unknown DNS server option for hostname ~s: ~p", [Hostname, Entry]),
                {error, {unknown_option, Entry}}
        end
    end, Config),
    flatten_servers(Rest, [Hosts, Result]);
flatten_servers([], Result) ->
    lists:flatten(Result).

%% @private
do_one_ipv4_host(Hostname) ->
    lager:notice("Trying to discover IPv4 peers via DNS: ~s", [Hostname]),
    case inet_res:getbyname(Hostname, a) of
        {ok, #hostent { h_addrtype = inet, h_addr_list = IPs }} ->
            IPs;
        {error, Reason} ->
            lager:error("Error: Unable to discover IPv4 peers via DNS (~s): ~p", [Hostname, Reason]),
            []
    end.

%% @private
do_one_ipv6_host(Hostname) ->
    lager:notice("Trying to discover IPv6 peers via DNS: ~s", [Hostname]),
    case inet_res:getbyname(Hostname, aaaa) of
        {ok, #hostent { h_addrtype = inet6, h_addr_list = IPs }} ->
            IPs;
        {error, Reason} ->
            lager:error("Error: Unable to discover IPv6 peers via DNS (~s): ~p", [Hostname, Reason]),
            []
    end.
