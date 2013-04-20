%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc Bitcoin Block Index Server.
-module(peculium_block_index_srv).

%% API.
-export([start_link/0, insert/1, best_block_index/0, best_block_height/0, height_to_hash/1, exists/1]).

%% Callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("peculium/include/peculium.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    %% Hash to block_index_entry mapping.
    block_index_map :: ets:tid(),

    %% Height to hash.
    height_map :: ets:tid(),

    %% The hash of the current best block.
    best_block_hash = undefined :: binary() | undefined
}).

%% @doc Start Block Index Server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if a given hash exists in the index.
-spec exists(Hash :: binary()) -> boolean().
exists(Hash) ->
    gen_server:call(?SERVER, {exists, Hash}).

%% @doc Insert block into the block index.
insert(Block) ->
    gen_server:call(?SERVER, {insert, Block}, infinity).

%% @doc Get best block index entry.
best_block_index() ->
    gen_server:call(?SERVER, best_block_index).

%% @doc Get best block's height.
-spec best_block_index() -> non_neg_integer().
best_block_height() ->
    case best_block_index() of
        not_found ->
            {error, not_found};
        BlockIndexEntry ->
            {ok, peculium_block_index:height(BlockIndexEntry)}
    end.

%% @doc Get block hash from height.
-spec height_to_hash(non_neg_integer()) -> {ok, binary()} | {error, any()}.
height_to_hash(Height) ->
    gen_server:call(?SERVER, {height_to_hash, Height}).

%% @private
init([]) ->
    {ok, #state {
        block_index_map = ets:new(block_index_map, [set, {keypos, #block_index_entry.hash}]),
        height_map = ets:new(height_map, [set])
    } }.

%% @private
handle_call({insert, Block}, _From, #state { height_map = HeightMap, block_index_map = Map, best_block_hash = BestBlockHash } = State) ->
    BlockIndexEntry = peculium_block_index:from_block(Block),
    BlockHash = peculium_block_index:hash(BlockIndexEntry),
    PreviousBlockHash = peculium_block_index:previous(BlockIndexEntry),
    case BestBlockHash of
        undefined ->
            %% Genesis block.
            ets:insert(Map, BlockIndexEntry#block_index_entry { height = 0, previous = undefined }),
            ets:insert(HeightMap, {0, BlockHash}),
            peculium_block_store_srv:put(peculium_block:hash(Block), Block),
            {reply, ok, State#state { best_block_hash = BlockHash }};
        PreviousBlockHash ->
            %% Continuation of the chain.
            [OldBlockIndexEntry] = ets:lookup(Map, PreviousBlockHash),
            Height = peculium_block_index:height(OldBlockIndexEntry) + 1,
            lager:debug("Continuation block: ~s (~B)", [peculium_utilities:bin2hex(BlockHash), Height]),
            ets:insert(Map, [
                %% Update next pointer for previous entry.
                OldBlockIndexEntry#block_index_entry{
                    next = BlockHash
                },
                %% New block.
                BlockIndexEntry#block_index_entry{
                    previous = PreviousBlockHash,
                    height = Height
                }
            ]),
            ets:insert(HeightMap, {Height, BlockHash}),
            peculium_block_store_srv:put(peculium_block:hash(Block), Block),
            {reply, ok, State#state { best_block_hash = BlockHash }};
        _Otherwise ->
            lager:debug("Orphan block: ~s", [peculium_utilities:bin2hex(BlockHash)]),
            {reply, error, State}
    end;
handle_call({exists, Hash}, _From, #state { block_index_map = Map } = State) ->
    case ets:lookup(Map, Hash) of
        [_] ->
            {reply, true, State};
        [] ->
            {reply, false, State}
    end;
handle_call(best_block_index, _From, #state { block_index_map = Map, best_block_hash = BestBlockHash } = State) ->
    case ets:lookup(Map, BestBlockHash) of
        [BlockIndexEntry] ->
            {reply, BlockIndexEntry, State};
        [] ->
            {reply, not_found, State}
    end;
handle_call({height_to_hash, Height}, _From, #state { height_map = HeightMap } = State) ->
    case ets:lookup(HeightMap, Height) of
        [{_, Hash}] ->
            {reply, {ok, Hash}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
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
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
