[![Build Status](https://travis-ci.org/RumataEstor/lazy_state.svg?branch=master)](https://travis-ci.org/RumataEstor/lazy_state)


The problem
=====

I had to write a lot of cowboy_rest handlers. They allow you to abstract HTTP
specifics away and describe a resource as a set of callbacks some of which
would be called when needed.

Every callback accepts a user defined state that may store anything a developer
needs and generally it stores values that callbacks might use. For the sake of
efficiency or because of business requirements some values that might be used by
many callbacks need to be evaluated only once.

*And here comes the problem - a developer needs to know the first of the
callbacks that use such a value.*

This is generally determined by looking at the `REST flowcharts`
(http://ninenines.eu/docs/en/cowboy/HEAD/guide/rest_flowcharts/)
and the code that computes the value is put into that callback.
Then the value is stored in the new version of the state that is returned from
the callback and the rest of the callbacks get that state passed and read the
value from it instead of computing it again.

The solution
====

A developer writes a function describing how to calculate a value and which
values are necessary for that. When a value is required the dependencies are
resolved automatically and they are applied to the function that produces the
value.

This way a developer only worries about *HOW* to calculate a value rather than
*IN WHAT ORDER*.

How to use
=====

It is convenient when a producing function may signal that the value could not
be produced, so all the values depending on this could also not be produced.

This is achieved by requiring producers to produce either `{ok, Value}` or
`{error, Reason}`.

```erlang
    State = lazy_state:new([{a, 1},
                            {b, [a], fun(A) -> {ok, A + 1} end}]),
    {{ok, 2}, _State1} = lazy_state:get(b, State).
```

In most cases you want to call a function providing arguments if they were
resolved successfully.

```erlang
    State = lazy_state:new([{a, 1},
                            {b, [a], fun(A) -> {ok, A + 1} end},
                            {c, [a, b], fun(A, B) -> {ok, {A, B}} end}]),
    {{1, 2}, _State1} = lazy_state:inject([c], fun(C) -> C end, State).
```

If a value can't be resolved the attempt to do so produces an error.

```erlang
    State = lazy_state:new([{a, [], fun() -> {ok, 1} end},
                            {b, [a, c], fun(A, C) -> {ok, {A, C}} end}]),
    {{error, {unresolved, b,
              {unresolved, c, notfound}}},
     _State1} = lazy_state:inject([b], fun(B) -> B end, State).
```

Build
-----

    $ rebar3 compile
    $ rebar3 eunit
