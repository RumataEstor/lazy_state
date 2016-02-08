-module(lazy_state_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

value_test() ->
    Key = a,
    Value = 1,
    State = lazy_state:new([lazy_state:value(Key, Value)]),
    ?assertMatch({ok, Value}, lazy_state:peek(Key, State)),
    {{ok, Value}, NewState} = lazy_state:get(Key, State).
