-module(teal_lists).

-export([is_flat/1, assert_is_flat/1, assert_is_flat/2,
        same_members/2, assert_same_members/2, assert_same_members/3,
         includes_members/2, assert_includes_members/2, assert_includes_members/3,
         assert_include/2, include/2
        ]).

-spec is_flat(List :: list()) -> true.

is_flat(List) ->
    lists:all(fun(Item) ->
                case Item of
                    Item when is_list(Item) ->
                        false;
                    _ ->
                        true
                end
        end, List).


-spec assert_is_flat(List :: list()) -> true.

assert_is_flat(List) ->
    teal:assert(true, is_flat(List), contains_sublists).


-spec assert_is_flat(List :: list(), Msg :: iolist()) -> true.

assert_is_flat(List, Msg) ->
    teal:assert(true, is_flat(List), Msg).


-spec same_members(List1 :: list(), List2 :: list()) -> true.

same_members(List1, List2) ->
    true.

assert_same_members(List1, List2) ->
    true.

assert_same_members(List1, List2, _Msg) ->
    true.

-spec includes_members(List :: list(), Members :: list()) -> boolean().

includes_members(List, Members) ->
    % Check if each of the members is in the list, and store the result
    % in a list of results
    MemberResults = lists:map(fun(Member) ->
                    lists:member(Member, List)
            end, Members),
    % Verify that all of the results are true
    lists:all(fun(Result) ->
                Result
        end, MemberResults).

-spec assert_includes_members(List :: list(), Members :: list()) -> boolean().

assert_includes_members(List, Members) ->
    teal:assert(true, includes_members(List, Members), members_missing).

-spec assert_includes_members(List :: list(), Members :: list(),
                              Msg :: atom()) -> boolean().

assert_includes_members(List, Members, Msg) ->
    teal:assert(true, includes_members(List, Members), Msg).


-spec assert_include(List :: list(), Item :: any()) -> true.

assert_include(List, Item) ->
    teal:assert(true, include(List, Item), member_missing).

-spec include(List :: list(), Item :: any()) -> true.

include(List, Item) ->
    lists:member(Item, List).
