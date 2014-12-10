-module(teal_types).

-export([not_of_type/2,
         not_record/1, assert_not_record/1, assert_not_record/2,
         could_be_record/1, assert_could_be_record/1, assert_could_be_record/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

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


-spec could_be_record(Record :: any()) -> boolean().

could_be_record(Record) ->
    % Check if term is a tuple with an atom as the first item
    case is_tuple(Record) of
        true ->
            % Check if the first item is an atom
            First = erlang:element(1, Record),
            is_atom(First);
        false ->
            false
    end.

-spec assert_could_be_record(Record :: any()) -> boolean().

assert_could_be_record(Record) ->
    teal:assert(true, could_be_record(Record), not_record).

-spec assert_could_be_record(Record :: any(), Msg :: any()) -> boolean().

assert_could_be_record(Record, Msg) ->
    teal:assert(true, could_be_record(Record), Msg).
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
