%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Store Server.
-module(peculium_block_store_srv).

%% API.
-export([start_link/0, exists/1, put/2, delete/1, get/1]).

%% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("peculium/include/peculium.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    hanoidb :: hanoidb:hanoidb()
}).

%% @doc Check if a given block exists in the store.
-spec exists(Hash :: binary()) -> boolean().
exists(Hash) ->
    gen_server:call(?SERVER, {exists, Hash}).

%% @doc Insert block.
-spec put(Hash :: binary(), Block :: bitcoin_block_message()) -> ok.
put(Hash, Block)->
    gen_server:cast(?SERVER, {put, Hash, Block}).

%% @doc Delete block.
-spec delete(Hash :: binary()) -> ok.
delete(Hash) ->
    gen_server:cast(?SERVER, {delete, Hash}).

%% @doc Get block.
-spec get(Hash :: binary()) -> {ok, bitcoin_block_message()} | {error, not_found}.
get(Hash) ->
    gen_server:call(?SERVER, {get, Hash}).

%% @doc Start Block Store Server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @private
init([]) ->
    lager:info("Starting Block Store Server"),
    {ok, HanoiDB} = hanoidb:open_link("/Users/ahf/peculium/", []),
    {ok, #state {
        hanoidb = HanoiDB
    }}.

%% @private
handle_call({exists, Hash}, _from, #state { hanoidb = HanoiDB } = State) ->
    case hanoidb:get(HanoiDB, Hash) of
        {ok, _} ->
            {reply, true, State};
        not_found ->
            {reply, false, State}
    end;
handle_call({get, Hash}, _from, #state { hanoidb = HanoiDB } = State) ->
    case hanoidb:get(HanoiDB, Hash) of
        {ok, Block} ->
            {reply, {ok, binary_to_term(Block)}, State};
        not_found ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast({delete, Hash}, #state { hanoidb = HanoiDB } = State) ->
    hanoidb:delete(HanoiDB, Hash),
    {noreply, State};
handle_cast({put, Hash, Block}, #state { hanoidb = HanoiDB } = State) ->
    lager:info("Adding ~s to Block Store", [binary_to_list(peculium_utilities:bin2hex(Hash))]),
    hanoidb:put(HanoiDB, Hash, term_to_binary(Block)),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state { hanoidb = HanoiDB }) ->
    lager:info("Stopping Block Store Server"),
    hanoidb:close(HanoiDB),
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
