-module(lazy_state_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(value_test(Key, Value, First, Last),
        ?_test(value_test(Key, Value, First, Last))).

-define(producer_test(Key, Keys, Producer, First, Last, Value),
        ?_test(producer_test(Key, Keys, Producer, First, Last, Value))).


value_test_() ->
    [
     ?value_test(a, 1, [], []),
     ?value_test(a, 1, [], [{b, 2}]),
     ?value_test(a, 1, [{b, 2}], []),
     ?value_test(a, 1, [{b, 2}], [{c, 3}])
    ].


make_counter(Ref) ->
    Self = self(),
    fun(X) -> Self ! {Ref, X}, Ref end.


ensure(Ref, List) ->
    receive
        {Ref, X} ->
            [X | Rest] = List,
            ensure(Ref, Rest)
    after 0 ->
            [] = List
    end.


value_test(Key, Value, First, Last) ->
    State = lazy_state:new(First ++ [{Key, Value}] ++ Last),
    Ref = make_ref(),

    ?assertMatch({ok, Value}, lazy_state:peek(Key, State)),
    ?assertMatch({{ok, Value}, State}, lazy_state:get(Key, State)),

    ?assertMatch({Ref, State}, lazy_state:inject([Key], make_counter(Ref), State)),
    ensure(Ref, [Value]),

    ?assertMatch({{error, {cant_resolve, inexistent_key, not_found}},
                  State}, lazy_state:inject([inexistent_key], make_counter(Ref), State)),
    ensure(Ref, []),
    ok.


producer_test_() ->
    [
     ?producer_test(a, [b], fun(B) -> {ok, B + 1} end, [{b, 1}], [{c, 2}], 2),
     ?producer_test(a, [b], fun(B) -> {ok, B + 1} end, [{c, 2}], [{b, 1}], 2),
     ?producer_test(a, [c, b], fun(C, B) -> {ok, {C, B}} end, [{c, 2}], [{b, 1}], {2, 1})
    ].

bad_producer_test() ->
    State = lazy_state:new([{a, [b], fun(B) -> {ok, B} end}]),
    Ref = make_ref(),

    {{error, {cant_resolve, a, {cant_resolve, b, not_found}}}, _State} = lazy_state:inject([a], make_counter(Ref), State),
    ensure(Ref, []),
    ok.

producer_test(Key, Keys, Producer, First, Last, Value) ->
    State = lazy_state:new(First ++ [{Key, Keys, Producer}] ++ Last),
    Ref = make_ref(),

    ?assertMatch({error, not_resolved}, lazy_state:peek(Key, State)),
    {Ref, NewState} = lazy_state:inject([Key], make_counter(Ref), State),
    ensure(Ref, [Value]),
    ?assertMatch({ok, Value}, lazy_state:peek(Key, NewState)),

    ?assertMatch({{error, {cant_resolve, inexistent_key, not_found}}, State}, lazy_state:inject([inexistent_key], make_counter(Ref), State)),
    ensure(Ref, []),

    ok.


example1_test() ->
    State = lazy_state:new([{a, 1},
                            {b, [a], fun(A) -> {ok, A + 1} end}]),
    {{ok, 2}, _State1} = lazy_state:get(b, State).


example2_test() ->
    State = lazy_state:new([{a, 1},
                            {b, [a], fun(A) -> {ok, A + 1} end},
                            {c, [a, b], fun(A, B) -> {ok, {A, B}} end}]),
    {{1, 2}, _State1} = lazy_state:inject([c], fun(C) -> C end, State).

example3_test() ->
    State = lazy_state:new([{a, [], fun() -> {ok, 1} end},
                            {b, [a, c], fun(A, C) -> {ok, {A, C}} end}]),
    {{error, {cant_resolve, b,
              {cant_resolve, c, not_found}}},
     _State1} = lazy_state:inject([b], fun(B) -> B end, State).
