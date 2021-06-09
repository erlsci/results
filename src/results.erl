-module(results).

-compile({no_auto_import, [error/1]}).

-export([new/0, new/1, new/2]).
-export([value/1, values/1, error/1, errors/1]).
-export([has_error/1, has_errors/1]).

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

-spec value(result()) -> term().
value(#result{value = V}) ->
  V.

-spec values([result()]) -> [term()].
values(Results) ->
  [value(R) || R <- Results].

-spec error(result()) -> term().
error(#result{error = E}) ->
  E.

-spec errors([result()]) -> [term()].
errors(Results) ->
  [error(R) || R <- Results].

-spec has_error(result()) -> boolean().
has_error(#result{error = E}) when E =:= undefined ->
    false;
has_error(_) ->
    true.

-spec has_errors([result()]) -> [boolean()].
has_errors(Results) ->
    [has_error(R) || R <- Results].
