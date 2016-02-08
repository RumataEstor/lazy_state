-module(lazy_state).

-export([new/1, value/2, producer/3]).
-export([inject/3, get/2, peek/2]).

-export_type([state/0, item/0]).

-type key() :: atom().
-type value() :: any().
-type receiver(V) :: fun((...) -> V).
-type maybe_value() :: {ok, value()} | {error, term()}.
-type producer() :: receiver(maybe_value()).

-opaque item() :: {key(), value()} | {key(), [key()], producer()}.
-opaque state() :: [item()].



-spec new([item()]) -> state().
new(Items) ->
    Items.


-spec value(key(), value()) -> item().
value(Key, Value) ->
    {Key, {ok, Value}}.


-spec producer(key(), [key()], producer()) -> item().
producer(Key, Keys, Producer) ->
    {Key, Keys, Producer}.


-spec inject([key()], receiver(V), state()) -> {V, state()}.
inject(Keys, Receiver, State) ->
    inject(Keys, [], State, Receiver).


inject([Key | Keys], RevValues, State, Receiver) ->
    case get(Key, State) of
        {{ok, Value}, NewState} ->
            inject(Keys, [Value | RevValues], NewState, Receiver);
        Error ->
            Error
    end;

inject([], RevValues, State, Receiver) ->
    {erlang:apply(Receiver, lists:reverse(RevValues)), State}.


-spec get(key(), state()) -> {maybe_value(), state()}.
get(Key, State) ->
    case lists:keyfind(Key, 1, State) of
        {_, Value} ->
            {Value, State};

        {_, Keys, Producer} ->
            {Value, NewState} = inject(Keys, Producer,
                                       lists:keydelete(Key, 1, State)),
            {Value, [{Key, Value} | NewState]};

        _ ->
            throw({notfound, Key, State})
    end.


-spec peek(key(), state()) -> maybe_value().
peek(Key, State) ->
    case lists:keyfind(Key, 1, State) of
        {_, Value} ->
            Value;
        {_, _, _} ->
            {error, not_resolved};
        _ ->
            {error, not_found}
    end.
