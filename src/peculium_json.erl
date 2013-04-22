%% @author Alexander Færøy <ahf@0x90.dk>
%% @copyright 2013 Alexander Færøy
%% @doc JSON Utilities
-module(peculium_json).

%% API.
-export([encode/1]).

-include_lib("peculium/include/peculium.hrl").

%% @doc Serialize Bitcoin message to JSON.
encode(#bitcoin_message { header = Header, body = Body }) ->
    [
        {<<"header">>, encode(Header)},
        {<<"body">>, encode(Body)}
    ];

encode(#bitcoin_message_header { network = Network, command = Command, length = Length, checksum = Checksum, valid = Valid }) ->
    [
        {<<"network">>, atom_to_binary(Network, utf8)},
        {<<"command">>, atom_to_binary(Command, utf8)},
        {<<"length">>, Length},
        {<<"checksum">>, peculium_utilities:bin2hex(Checksum)},
        {<<"valid">>, Valid}
    ];

encode(#bitcoin_verack_message {}) ->
    null;

encode(#bitcoin_ping_message {}) ->
    null;

encode(#bitcoin_getaddr_message {}) ->
    null;

encode(#bitcoin_version_message { version = Version, services = Services, timestamp = Timestamp, to_address = ToAddress, from_address = FromAddress, user_agent = UserAgent, start_height = StartHeight, relay = Relay, nonce = Nonce }) ->
    [
        {<<"version">>, Version},
        {<<"services">>, Services},
        {<<"timestamp">>, Timestamp},
        {<<"to_address">>, encode(ToAddress)},
        {<<"from_address">>, encode(FromAddress)},
        {<<"user_agent">>, UserAgent},
        {<<"start_height">>, StartHeight},
        {<<"relay">>, Relay},
        {<<"nonce">>, peculium_utilities:bin2hex(Nonce)}
    ];

encode(#bitcoin_alert_message { payload = Payload, signature = Signature }) ->
    [
        {<<"payload">>, peculium_utilities:bin2hex(Payload)},
        {<<"signature">>, peculium_utilities:bin2hex(Signature)}
    ];

encode(#bitcoin_inv_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#bitcoin_getdata_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#bitcoin_notfound_message { inventory = Inventory }) ->
    [
        {<<"inventory">>, lists:map(fun encode/1, Inventory)}
    ];

encode(#bitcoin_addr_message { addresses = Addresses }) ->
    [
        {<<"addresses">>, lists:map(fun encode/1, Addresses)}
    ];

encode(#bitcoin_headers_message { headers = Headers }) ->
    [
        {<<"headers">>, lists:map(fun encode/1, Headers)}
    ];

encode(#bitcoin_getblocks_message { version = Version, block_locator_hashes = BlockLocatorHashes, hash_stop = HashStop }) ->
    [
        {<<"version">>, Version},
        {<<"block_locator_hashes">>, lists:map(fun peculium_utilities:bin2hex/1, BlockLocatorHashes)},
        {<<"hash_stop">>, peculium_utilities:bin2hex(HashStop)}
    ];

encode(#bitcoin_getheaders_message { version = Version, block_locator_hashes = BlockLocatorHashes, hash_stop = HashStop }) ->
    [
        {<<"version">>, Version},
        {<<"block_locator_hashes">>, lists:map(fun peculium_utilities:bin2hex/1, BlockLocatorHashes)},
        {<<"hash_stop">>, peculium_utilities:bin2hex(HashStop)}
    ];

encode(#bitcoin_transaction { version = Version, transaction_inputs = Inputs, transaction_outputs = Outputs, lock_time = LockTime }) ->
    [
        {<<"version">>, Version},
        {<<"transaction_inputs">>, lists:map(fun encode/1, Inputs)},
        {<<"transaction_outputs">>, lists:map(fun encode/1, Outputs)},
        {<<"lock_time">>, LockTime}
    ];

encode(#bitcoin_block { version = Version, previous_block = PreviousBlock, merkle_root = MerkleRoot, timestamp = Timestamp, bits = Bits, nonce = Nonce, transactions = Transactions }) ->
    [
        {<<"version">>, Version},
        {<<"previous_block">>, peculium_utilities:bin2hex(PreviousBlock)},
        {<<"merkle_root">>, peculium_utilities:bin2hex(MerkleRoot)},
        {<<"timestamp">>, Timestamp},
        {<<"bits">>, Bits},
        {<<"nonce">>, Nonce},
        {<<"transactions">>, lists:map(fun encode/1, Transactions)}
    ];

encode(#bitcoin_transaction_outpoint { index = Index, hash = Hash }) ->
    [
        {<<"index">>, Index},
        {<<"hash">>, peculium_utilities:bin2hex(Hash)}
    ];

encode(#bitcoin_transaction_input { previous_output = PreviousOutput, script = Script, sequence = Sequence }) ->
    [
        {<<"previous_output">>, encode(PreviousOutput)},
        {<<"script">>, peculium_utilities:bin2hex(Script)},
        {<<"sequence">>, Sequence}
    ];

encode(#bitcoin_transaction_output { value = Value, script = Script }) ->
    [
        {<<"value">>, Value},
        {<<"script">>, peculium_utilities:bin2hex(Script)}
    ];

encode(#bitcoin_inv { type = Type, hash = Hash }) ->
    [
        {<<"type">>, atom_to_binary(Type, utf8)},
        {<<"hash">>, peculium_utilities:bin2hex(Hash)}
    ];

encode(#bitcoin_network_address { time = Time, services = Services, address = Address, port = Port }) ->
    [
        {<<"time">>, Time},
        {<<"services">>, Services},
        {<<"address">>, list_to_binary(inet_parse:ntoa(Address))},
        {<<"port">>, Port}
    ].
