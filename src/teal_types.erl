-module(teal_types).

-export([not_record/1]).

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


%%%===================================================================
%%% Private functions
%%%===================================================================
