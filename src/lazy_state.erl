-module(lazy_state).

-export([new/1]).
-export([inject/3, get/2, peek/2]).

-export_type([state/0, item/0]).

-type key() :: atom().
-type value() :: any().
-type receiver(V) :: fun((...) -> V).
-type maybe_value() :: {ok, value()} | {error, term()}.
-type producer() :: receiver(maybe_value()).

-type item() :: {key(), value()} | {key(), [key()], producer()}.

%% dialyzer opaque bug workaround
-type state_() :: [{key(), maybe_value()}].
-opaque state() :: state_().


-spec new([item()]) -> state().
new(Items) ->
    lists:map(fun new_item/1, Items).


-spec new_item({key(), value()}) -> item();
              ({key(), [key()], producer()}) -> item().
new_item({Key, Value}) ->
    {Key, {ok, Value}};

new_item({Key, Keys, Producer}) when is_function(Producer, length(Keys)) ->
    {Key, Keys, Producer}.


-spec inject([key()], receiver(V), state_()) -> {V, state()} | {{error, term()}, state()}.
inject(Keys, Receiver, State) ->
    inject(Keys, [], State, Receiver).


inject([Key | Keys], RevValues, State, Receiver) ->
    case get(Key, State) of
        {{ok, Value}, NewState} ->
            inject(Keys, [Value | RevValues], NewState, Receiver);
        {{error, Reason}, NewState} ->
            {{error, {unresolved, Key, Reason}}, NewState}
    end;

inject([], RevValues, State, Receiver) ->
    {erlang:apply(Receiver, lists:reverse(RevValues)), State}.


-spec get(key(), state_()) -> {maybe_value(), state()}.
get(Key, State) ->
    case lists:keyfind(Key, 1, State) of
        {_, Value} ->
            {Value, State};

        {_, Keys, Producer} ->
            {Value, NewState} = inject(Keys, Producer,
                                       lists:keydelete(Key, 1, State)),
            {Value, [{Key, Value} | NewState]};

        _ ->
            {{error, notfound}, State}
    end.


-spec peek(key(), state()) -> maybe_value().
peek(Key, State) ->
    case lists:keyfind(Key, 1, State) of
        {_, Value} ->
            Value;
        {_, _, _} ->
            {error, unresolved};
        _ ->
            {error, notfound}
    end.
