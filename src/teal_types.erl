-module(teal_types).

-export([not_record/1, assert_not_record/1, assert_not_record/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec not_record(Term :: any()) -> boolean().

not_record(Term) ->
    case is_tuple(Term) of
        true ->
            case is_atom(element(1, Term)) of
                true ->
                    false;
                false ->
                    true
            end;
        false ->
            true
    end.

-spec assert_not_record(Term :: any()) -> boolean().

assert_not_record(Term) ->
    teal:assert(true, not_record(Term),
                is_a_record).

-spec assert_not_record(Term :: any(), Msg :: any()) -> boolean().

assert_not_record(Term, Msg) ->
    teal:assert(true, not_record(Term), Msg).

%%%===================================================================
%%% Private functions
%%%===================================================================
