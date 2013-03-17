-define(PECULIUM_VERSION, <<"0.1.0">>).
-define(PECULIUM_USER_AGENT, [<<"peculium/", ?PECULIUM_VERSION>>]).

-type bitcoin_unit_atom() :: megabitcoin | kilobitcoin | hectobitcoin | decabitcoin
                           | bitcoin | decibitcoin | centibitcoin | millibitcoin
                           | microbitcoin | satoshi.

-type uint8_t()  :: 0..255.
-type uint16_t() :: 0..65535.
-type uint32_t() :: 0..4294967295.
-type uint64_t() :: 0..18446744073709551615.

-type int8_t()  :: integer().
-type int16_t() :: integer().
-type int32_t() :: integer().
-type int64_t() :: integer().

-type bitcoin_network_atom() :: mainnet | testnet | testnet3.

-type bitcoin_command_atom() :: addr | alert | block | checkorder
                              | getaddr | getblocks | getdata | getheaders
                              | headers | inv | ping | submitorder
                              | reply | tx | verack | version.

-type bitcoin_inv_atom() :: error | tx | block.
-type bitcoin_inv_integer() :: 0 | 1 | 2.

-type bitcoin_checksum() :: <<_:32>>.

-record(bitcoin_message_header, {
    network :: bitcoin_network_atom(),
    command :: bitcoin_command_atom(),
    length :: uint32_t(),
    checksum :: bitcoin_checksum()
}).

-record(bitcoin_message, {}).
