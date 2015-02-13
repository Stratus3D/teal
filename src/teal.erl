-module(teal).

-export([assert/3,
         not_equal/2, assert_not_equal/2, assert_not_equal/3,
         raises_exception/1, assert_raises_exception/1,
         assert_raises_exception/2,
         raises_throw/1, raises_throw_with_message/2,
         assert_raises_throw/1, assert_raises_throw/2,
         assert_raises_throw_with_message/2, assert_raises_throw_with_message/3,
         raises_error/1,
         raises_exit/1
         ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec assert(Lhs :: any(), Rhs :: any(), Message :: atom()) -> true.

assert(Lhs, Rhs, Message) ->
    try Lhs = Rhs of
        Lhs -> true
    catch
        error:{badmatch, _} ->
            erlang:error(Message)
    end.

%% equality functions
-spec not_equal(Term1 :: any(), Term2 :: any()) -> boolean().

not_equal(Term1, Term2) ->
    case Term1 of
        Term2 ->
            false;
        _ ->
            true
    end.

-spec assert_not_equal(Term1 :: any(), Term2 :: any()) -> boolean().

assert_not_equal(Term1, Term2) ->
    teal:assert(true, not_equal(Term1, Term2), equal).

-spec assert_not_equal(Term1 :: any(), Term2 :: any(), Msg :: atom()) -> boolean().

assert_not_equal(Term1, Term2, Msg) ->
    teal:assert(true, not_equal(Term1, Term2), Msg).

%% Exception function
-spec raises_exception(Fun :: fun()) -> boolean().

raises_exception(Fun) when is_function(Fun) ->
    does_raise_exception(Fun).

-spec assert_raises_exception(Fun :: fun()) -> boolean().

assert_raises_exception(Fun) ->
    assert(true, raises_exception(Fun), no_exception_caught).

-spec assert_raises_exception(Fun :: fun(), Msg :: any()) -> boolean().

assert_raises_exception(Fun, Msg) ->
    assert(true, raises_exception(Fun), Msg).

-spec raises_throw(Fun :: fun()) -> boolean().

raises_throw(Fun) when is_function(Fun) ->
    does_raise_exception(Fun, throw).

-spec raises_throw_with_message(Fun :: fun(), ErrMsg :: any()) -> boolean().

raises_throw_with_message(Fun, ErrMsg) when is_function(Fun) ->
    does_raise_exception(Fun, throw, ErrMsg).

-spec assert_raises_throw(Fun :: fun()) -> boolean().

assert_raises_throw(Fun) ->
    assert(true, raises_throw(Fun), no_exception_caught).

-spec assert_raises_throw(Fun :: fun(), Msg :: any()) -> boolean().

assert_raises_throw(Fun, Msg) ->
    assert(true, raises_throw(Fun), Msg).

-spec assert_raises_throw_with_message(Fun :: fun(), ErrMsg :: any()) -> boolean().

assert_raises_throw_with_message(Fun, ErrMsg) ->
    assert(true, raises_throw_with_message(Fun, ErrMsg), no_exception_caught).

-spec assert_raises_throw_with_message(Fun :: fun(), ErrMsg :: any(), Msg :: any()) -> boolean().

assert_raises_throw_with_message(Fun, ErrMsg, Msg) ->
    assert(true, raises_throw_with_message(Fun, ErrMsg), Msg).

-spec raises_error(Fun :: fun()) -> boolean().

raises_error(Fun) when is_function(Fun) ->
    does_raise_exception(Fun, error).

-spec raises_exit(Fun :: fun()) -> boolean().

raises_exit(Fun) when is_function(Fun) ->
    does_raise_exception(Fun, exit).

%%%===================================================================
%%% Private functions
%%%===================================================================
does_raise_exception(Fun) when is_function(Fun) ->
    try Fun() of
        _ -> false
    catch
        _Error:_ErrMsg ->
            true
    end.

does_raise_exception(Fun, Error) when is_function(Fun) ->
    try Fun() of
        _ -> false
    catch
        Error:_ErrMsg ->
            true
    end.

does_raise_exception(Fun, Error, ErrorMessage) when is_function(Fun) ->
    try Fun() of
        _ -> false
    catch
        Error:ErrorMessage ->
            true
    end.
