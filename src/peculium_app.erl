-module(peculium_app).
-behaviour(application).

%% API.
-export([start/0, start/2, stop/1]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(peculium).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_, _) ->
    %% Start peer listeners.
    {ok, _} = ranch:start_listener(peculium_listener, 100,
        ranch_tcp, [{port, 5555}],
        peculium_peer, []),

    %% Set debug log level.
    lager:set_loglevel(lager_console_backend, debug),

    %% Start the supervisor.
    Result = peculium_sup:start_link(),

    %% Ensure that the dot directories exists.
    filelib:ensure_dir(peculium_config:dotdir() ++ "/"),
    filelib:ensure_dir(peculium_config:block_chain_dir() ++ "/"),

    Result.

-spec stop([]) -> ok.
stop(_State) ->
    ok.
