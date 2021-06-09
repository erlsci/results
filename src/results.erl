-module(results).

-compile({no_auto_import, [error/1]}).

-export([new/0, new/1, new/2, new_error/1]).
-export([value/1, values/1, error/1, errors/1]).
-export([has_value/1, has_values/1, has_error/1, has_errors/1]).
-export([r_and/2, r_or/2]).

-include_lib("include/results.hrl").

-spec new() -> result().
new() ->
    #result{}.

-spec new(term()) -> result().
new(Value) ->
  #result{
     value = Value
  }.

-spec new(term(), term()) -> result().
new(Value, Error) ->
  #result{
     value = Value,
     error = Error
  }.

-spec new_error(term()) -> result().
new_error(Error) ->
    new(undefined, Error).

-spec value(result()) -> term().
value(#result{value=V}) ->
  V.

-spec values([result()]) -> [term()].
values(Results) ->
  [value(R) || R <- Results].

-spec has_value(result()) -> boolean().
has_value(#result{value=V}) when V =:= undefined ->
    false;
has_value(_) ->
    true.

-spec has_values([result()]) -> [boolean()].
has_values(Results) ->
    [has_value(R) || R <- Results].

-spec error(result()) -> term().
error(#result{error=E}) ->
  E.

-spec errors([result()]) -> [term()].
errors(Results) ->
  [error(R) || R <- Results].

-spec has_error(result()) -> boolean().
has_error(#result{error=E}) when E =:= undefined ->
    false;
has_error(_) ->
    true.

-spec has_errors([result()]) -> [boolean()].
has_errors(Results) ->
    [has_error(R) || R <- Results].

-spec r_and(result(), result()) -> [term()].
r_and(#result{error=E1}, #result{error=E2}) when E1=/=undefined, E2=/=undefined ->
    [E1, E2];
r_and(#result{value=V1}, #result{value=V2}) when V1=/=undefined, V2=/=undefined ->
    [V1, V2];
r_and(_, _) ->
    [].

-spec r_or(result(), result()) -> [term()].
r_or(#result{error=E1}, #result{error=E2}) when E1=/=undefined orelse E2=/=undefined ->
    [E || E <- [E1, E2], E =/= undefined];
r_or(#result{value=V1}, #result{value=V2}) ->
    [V || V <- [V1, V2], V =/= undefined].
