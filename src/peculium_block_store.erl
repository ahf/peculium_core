-module(peculium_block_store).

-behaviour(gen_server).

-include_lib("peculium/include/peculium.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([add/1, contains/1, get/1]).

-define(SERVER, ?MODULE).

-record(state, {
    leveldb :: eleveldb:db_ref()
}).

add(Block) ->
    gen_server:cast(?SERVER, {add, Block}).

contains(Hash) ->
    gen_server:call(?SERVER, {contains, Hash}).

get(Hash) ->
    gen_server:call(?SERVER, {get, Hash}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    lager:debug("Starting block store"),
    {ok, DbRef} = eleveldb:open("/Users/ahf/peculium/blockstore", [{create_if_missing, true}]),
    {ok, #state {
        leveldb = DbRef
    }}.

handle_call({contains, Hash}, _From, State) ->
    case eleveldb:get(State#state.leveldb, Hash, []) of
        {ok, _Block} ->
            {reply, true, State};
        _Otherwise ->
            {reply, false, State}
    end;

handle_call({get, Hash}, _From, State) ->
    case eleveldb:get(State#state.leveldb, Hash, []) of
        {ok, Block} ->
            {reply, binary_to_term(Block), State};
        _Otherwise ->
            {reply, not_found, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add, Block}, State) ->
    Hash = peculium_block:hash(Block),
    lager:debug("Saving block: ~s", [peculium_utilities:bin2hex(Hash)]),
    case eleveldb:put(State#state.leveldb, Hash, term_to_binary(Block), []) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
