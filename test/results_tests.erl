-module(results_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("include/results.hrl").

new_empty_test() ->
    R = results:new(),
    ?assertEqual(undefined, R#result.value).

new_result_value_test() ->
    R = results:new(alice),
    ?assertEqual(alice, R#result.value),
    ?assertEqual(undefined, R#result.error).

new_result_error_test() ->
    R1 = results:new(bob, {error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, R1#result.error),
    R2 = results:new(undefined, {error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, R2#result.error).

value_test() ->
    ?assertEqual(alice, results:value(results:new(alice))).

values_test() ->
    Rs = [results:new(alice),
          results:new(bob),
          results:new(carol)],
    ?assertEqual([alice,bob,carol], results:values(Rs)).

error_test() ->
    R1 = results:new(bob, {error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, results:error(R1)),
    R2 = results:new(undefined, oops),
    ?assertEqual(oops, results:error(R2)).

errors_test() ->
    Rs = [results:new(undefined, {error, absent_alice}),
          results:new(undefined, {error, bad_news_bob}),
          results:new(undefined, {error, collups_cutting_carol})],
    ?assertEqual([absent_alice,
                  bad_news_bob,
                  collups_cutting_carol],
                 [element(2, R) || R <- results:errors(Rs)]).

has_error_false_test() ->
    ?assertNot(results:has_error(results:new())),
    ?assertNot(results:has_error(results:new(alice))),
    ?assertNot(results:has_error(results:new(alice, undefined))).

has_error_true_test() ->
    ?assert(results:has_error(results:new(nil, oops))),
    ?assert(results:has_error(results:new(nil, {error, oops}))).

has_errors_false_test() ->
    Rs = [results:new(alice),
          results:new(bob),
          results:new(carol)],
    ?assertEqual([false,false,false], results:has_errors(Rs)).

has_errors_true_test() ->
    Rs = [results:new(undefined, {error, absent_alice}),
          results:new(undefined, {error, bad_news_bob}),
          results:new(undefined, {error, collups_cutting_carol})],
    ?assertEqual([true,true,true], results:has_errors(Rs)).
