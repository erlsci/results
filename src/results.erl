-module(results).

-export([new/1, new/2]).
-export([value/1, error/1]).

-include_lib("include/results.hrl").

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
value(Result) ->
  Result#result.value.

-spec error(result()) -> term().
error(Result) ->
  Result#result.error.
