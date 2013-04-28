-ifdef(TEST).

-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec test() -> any().

-spec property_test() -> any().
property_test() ->
    true == triq:check(?MODULE).

-endif.
