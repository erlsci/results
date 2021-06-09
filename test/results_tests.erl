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
    ?assertEqual({error, "AWOL again, Bob"}, R2#result.error),
    R3 = results:new_error({error, "AWOL again, Bob"}),
    ?assertEqual({error, "AWOL again, Bob"}, R3#result.error).

value_test() ->
    ?assertEqual(alice, results:value(results:new(alice))).

values_test() ->
    Rs = [results:new(alice),
          results:new(bob),
          results:new(carol)],
    ?assertEqual([alice,bob,carol], results:values(Rs)).

has_value_false_test() ->
    ?assertNot(results:has_value(results:new())),
    ?assertNot(results:has_value(results:new(undefined, oops))),
    ?assertNot(results:has_value(results:new(undefined, {error, oops}))).

has_value_true_test() ->
    ?assert(results:has_value(results:new(alice))),
    ?assert(results:has_value(results:new(alice, undefined))),
    ?assert(results:has_value(results:new(alice, {error, oops}))).

has_values_true_test() ->
    Rs = [results:new(alice),
          results:new(bob),
          results:new(carol)],
    ?assertEqual([true,true,true], results:has_values(Rs)).

has_values_false_test() ->
    Rs = [results:new(undefined, {error, absent_alice}),
          results:new(undefined, {error, bad_news_bob}),
          results:new(undefined, {error, collups_cutting_carol})],
    ?assertEqual([false,false,false], results:has_values(Rs)).

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

r_and_no_errors_test() ->
    ?assertEqual([], results:r_and(results:new(), results:new())),
    ?assertEqual([dave,eve], results:r_and(results:new(dave), results:new(eve))),
    ?assertEqual([], results:r_and(results:new(dave), results:new())),
    ?assertEqual([], results:r_and(results:new(), results:new(eve))).

r_and_errors_test() ->
    ?assertEqual([],
                  results:r_and(results:new(freya),
                                results:new(undefined, {error, no_greta}))),
    ?assertEqual([],
                  results:r_and(results:new(undefined, {error, no_freya}),
                                results:new())),
    ?assertEqual([],
                  results:r_and(results:new(),
                                results:new(undefined, {error, no_greta}))),
    ?assertEqual([{error, no_freya}, {error, no_greta}],
                 results:r_and(results:new(undefined, {error, no_freya}),
                               results:new(undefined, {error, no_greta}))).

r_or_no_errors_test() ->
    ?assertEqual([], results:r_and(results:new(), results:new())),
    ?assertEqual([dave,eve], results:r_or(results:new(dave), results:new(eve))),
    ?assertEqual([dave], results:r_or(results:new(dave), results:new())),
    ?assertEqual([eve], results:r_or(results:new(), results:new(eve))).

r_or_errors_test() ->
    ?assertEqual([{error, no_greta}], 
                  results:r_or(results:new(freya),
                               results:new(undefined, {error, no_greta}))),
    ?assertEqual([{error, no_freya}, {error, no_greta}],
                 results:r_or(results:new(undefined, {error, no_freya}),
                              results:new(undefined, {error, no_greta}))).
