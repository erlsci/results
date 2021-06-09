%% Results of coercion are wrapped into a result record; this is a pattern from
%% other projects / languages. See:
%% * result = value, errors, warnings: https://github.com/clojusc/results
%% * result = ok, err: https://doc.rust-lang.org/std/result/enum.Result.html
-record(result, {
  value :: term(),
  error :: term()
}).
-type result() :: #result{}.
