%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Configuration server.
-module(peculium_config).
-behaviour(gen_server).

%% API.
-export([start_link/0, dotdir/0, block_chain_dir/0, block_store_dir/0, block_index_dir/0, cache_size/0]).

%% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% @doc Get the configuration directory.
-spec dotdir() -> string().
dotdir() ->
    call(dotdir).

%% @doc Get the block chain directory.
-spec block_chain_dir() -> string().
block_chain_dir() ->
    filename:join([dotdir(), "blockchain"]).

%% @doc Get the block store directory.
-spec block_store_dir() -> string().
block_store_dir() ->
    filename:join([block_chain_dir(), "store"]).

%% @doc Get the block index directory.
-spec block_index_dir() -> string().
block_index_dir() ->
    filename:join([block_chain_dir(), "index"]).

%% @doc Get the cache size in bytes.
-spec cache_size() -> non_neg_integer().
cache_size() ->
    call(cache_size).

%% @doc Start the configuration server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
    lager:info("Starting configuration server"),
    {ok, #state {}}.

%% @private
handle_call({value, Key}, _From, State) ->
    {reply, value(Key), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    lager:info("Terminating configuration server"),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% @private
value(Key) ->
    case application:get_env(peculium, Key) of
        undefined ->
            not_found;
        {ok, Value} ->
            Value
    end.

%% @private
call(Key) ->
    case gen_server:call(?SERVER, {value, Key}) of
        not_found ->
            exit({missing_config_value, Key});
        Value ->
            Value
    end.
