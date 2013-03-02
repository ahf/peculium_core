-module(peculium_app).
-behaviour(application).

%% API.
-export([start/0, start/2, stop/1]).

-spec start() -> ok | {error, term()}.
start() ->
    application:start(peculium).

-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()}.
start(_, _) ->
    peculium_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.
