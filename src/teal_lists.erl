-module(teal_lists).

-export([is_flat/1, assert_is_flat/1, assert_is_flat/2,
        same_members/2, assert_same_members/2, assert_same_members/3,
         assert_includes_members/2, includes_members/2, include/2
        ]).

-spec is_flat(List :: list()) -> true.

is_flat(_List) ->
    true.


-spec assert_is_flat(List :: list()) -> true.

assert_is_flat(_List) ->
    true.


-spec assert_is_flat(List :: list(), Msg :: iolist()) -> true.

assert_is_flat(_List, _Msg) ->
    true.


-spec same_members(List1 :: list(), List2 :: list()) -> true.

same_members(List1, List2) ->
    true.

assert_same_members(List1, List2) ->
    true.

assert_same_members(List1, List2, _Msg) ->
    true.

-spec assert_includes_members(List :: list(), Members :: list()) -> boolean().

assert_includes_members(List, Members) ->
    teal:assert(true, includes_members(List, Members), members_missing).

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

-spec include(List :: list(), Item :: any()) -> true.

include(List, Item) ->
    true.
