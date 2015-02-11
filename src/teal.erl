-module(teal).

-export([assert/3,
         not_equal/2, assert_not_equal/2, assert_not_equal/3,
         raises_throw/1, raises_exception/1, raises_error/1,
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

-spec raises_exception(Fun :: fun()) -> boolean().

raises_exception(Fun) ->
    try Fun() of
        _ -> false
    catch
        _:_Err ->
            true
    end.

-spec raises_throw(Fun :: fun()) -> boolean().

raises_throw(Fun) ->
    try Fun() of
        _ -> false
    catch
        throw:_Err ->
            true
    end.

-spec raises_error(Fun :: fun()) -> boolean().

raises_error(Fun) ->
    try Fun() of
        _ -> false
    catch
        error:_Err ->
            true
    end.

-spec raises_exit(Fun :: fun()) -> boolean().

raises_exit(Fun) ->
    try Fun() of
        _ -> false
    catch
        exit:_Err ->
            true
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================
