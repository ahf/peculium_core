-module(peculium_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUPERVISOR, ?MODULE).

%% From supervisor.
-type startlink_err() :: {already_started, pid()} | shutdown | term().
-type startlink_ret() :: {ok, pid()} | ignore | {error, startlink_err()}.

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, non_neg_integer(), non_neg_integer()}, []}}.
init(_State) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(peculium_block_store, worker),
        ?CHILD(peculium_orphan_block_store, worker)
    ]}}.
