-module(results_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/results.hrl").

new_result_value_test() ->
    R = results:new(alice),
    ?assertEqual(alice, R#result.value),
    ?assertEqual(undefined, R#result.error).

new_result_error_test() ->
    R1 = results:new(bob, {error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, R1#result.error),
    R2 = results:new(undefined, {error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, R2#result.error).
