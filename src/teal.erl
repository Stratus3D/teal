-module(teal).

-export([assert/3,
         not_equal/2, assert_not_equal/2, assert_not_equal/3,
         not_of_type/2]).

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

-spec not_of_type(Term :: atom(), Type :: atom()) -> atom().

not_of_type(Term, Type) ->
    %% Check for special cases
    case Type of
        builtin ->
            not_implemented;
        record ->
            not_implemented;
        _ ->
            FunName = list_to_atom("is_" ++ atom_to_list(Type)),
            invert_boolean(apply(erlang, FunName, [Term]))
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================

-spec invert_boolean(Boolean :: boolean()) -> boolean().

invert_boolean(Boolean) ->
    case Boolean of
        true ->
            false;
        false ->
            true
    end.
