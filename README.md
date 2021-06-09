# results

*An Erlang library for unifying the treatment of results and associated errors*

[![Build Status][gh-actions-badge]][gh-actions]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

## Intro

In a language that is focused on data and messages, it makes sense for errors to be as
well. This library makes it trivial for errors to be treated as such, and just as importantly,
to be treated consistently across multiple problem domains (as well as different solution implementations). (When building large systems, unified/consistent results-handling (values and errors) is critical to code maintainability.)

## Usage

Creating new results:

``` erlang
1> R1 = results:new().
{result,undefined,undefined}
```

``` erlang
2> R2 = results:new(42).
{result,42,undefined}
```

``` erlang
3> R3 = results:new(undefined, {error, oops}).
{result,undefined,{error,oops}}
```

``` erlang
4> R4 = results:new_error(oops).
{result,undefined,oops}
```

Checking results:

``` erlang
5> results:value(R2).
42
6> results:values([R1,R2,R3,R4]).
[undefined,42,undefined,undefined]
7> results:has_value(R1).
false
```

``` erlang
8> results:error(R3).
{error,oops}
9> results:errors([R1,R2,R3,R4]).
[undefined,undefined,{error,oops},oops]
10> results:has_error(R4).
true
```

``` erlang
12> results:r_and(R1, R2).
[]
13> results:r_and(R3, R4).
[{error,oops},oops]
```

``` erlang
14> results:r_or(R1, R2).
[42]
15> results:r_or(R3, R4).
[{error,oops},oops]
```


## License [&#x219F;](#contents)

Copyright © 2021, Erlang-Aided Enrichment Center

Copyright © 2018, Clojure-Aided Enrichment Center

Copyright © 2018, NASA

Distributed under the Apache License, Version 2.0.

[//]: ---Named-Links---

[logo]: priv/images/logo.png
[logo-large]: priv/images/logo-large.png
[github]: https://github.com/erlsci/results
[gh-actions-badge]: https://github.com/erlsci/results/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/erlsci/results/actions
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2024-blue.svg
[versions]: https://github.com/erlsci/results/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/erlsci/results/tags
[github-tag-badge]: https://img.shields.io/github/tag/erlsci/results.svg
[github-downloads]: https://img.shields.io/github/downloads/erlsci/results/total.svg
