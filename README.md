Lazy state
=====

Have you ever written cowboy_rest handlers? In this case you most probably had
to look through `HTTP flowcharts` to learn what order the state has to be
updated in.

This library allows you to forget about the order, all you need it to describe
what arguments a function needs and how each of those arguments is produced.
Producing functions will be invoked only when needed caching the resulting
value.

Example
-----



Build
-----

    $ rebar3 compile
