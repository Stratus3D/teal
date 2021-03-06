-module(teal_lists_SUITE).

%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([test_is_flat/1, test_assert_is_flat/1, test_assert_is_flat_2/1,
         test_same_members/1, test_assert_same_members_2/1, test_assert_same_members_3/1,
         test_includes_members/1, test_assert_includes_members/1, test_assert_includes_members_3/1,
         test_include/1,
         test_assert_include/1,
         test_order/1, test_assert_order_2/1, test_assert_order_3/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_is_flat, test_assert_is_flat, test_assert_is_flat,
     test_same_members, test_assert_same_members_2, test_assert_same_members_3,
     test_includes_members, test_assert_includes_members, test_assert_includes_members_3,
     test_include, test_assert_include,
     test_order, test_assert_order_2, test_assert_order_3].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

test_is_flat(_Config) ->
    FlatList = [a,b,c,d,e,f],
    List = [a,b,c,[d,e,f],g],

    % Should return true when all items are in the list
    true = teal_lists:is_flat(FlatList),

    % Should return false when a item is not in the list
    false = teal_lists:is_flat(List).

test_assert_is_flat(_Config) ->
    List = [a,b,c,[d,e,f],g],

    % Should raise a contains_sublists error when list is not flat
    try teal_lists:assert_is_flat(List) of
        _ -> erlang:error(failed)
    catch
        error:contains_sublists ->
            true
    end.
test_assert_is_flat_2(_Config) ->
    List = [a,b,c,[d,e,f],g],
    Error = not_flat,

    % Should raise a contains_sublists error when list is not flat
    try teal_lists:assert_is_flat(List, Error) of
        _ -> erlang:error(failed)
    catch
        error:Error ->
            true
    end.

test_same_members(_Config) ->
    ListA = [1,2,3,4],
    ListB = [2,3,4,1],
    ListC = [1,2,3,3],

    % Should return true when all items are in the list
    true = teal_lists:same_members(ListA, ListB),

    % Should return false when a item is not in the list
    false = teal_lists:same_members(ListA, ListC).

test_assert_same_members_2(_Config) ->
    ListA = [1,2,3,4],
    ListB = [2,3,4,1],
    ListC = [1,2,3,3],

    % Should return true when all items are in the list
    true = teal_lists:assert_same_members(ListA, ListB),

    % Should raise an error when a item is not in the list
    try teal_lists:assert_same_members(ListA, ListC) of
        _ -> erlang:error(failed)
    catch
        error:not_same_members ->
            true
    end.

test_assert_same_members_3(_Config) ->
    ListA = [1,2,3,4],
    ListB = [2,3,4,1],
    ListC = [1,2,3,3],
    Msg = test,

    % Should return true when all items are in the list
    true = teal_lists:assert_same_members(ListA, ListB, Msg),

    % Should raise an error when a item is not in the list
    try teal_lists:assert_same_members(ListA, ListC, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.

test_includes_members(_Config) ->
    List = [a,b,c,d,e,f],

    % Should return true when all items are in the list
    true = teal_lists:includes_members(List, [a,b,e]),

    % Should return false when a item is not in the list
    false = teal_lists:includes_members(List, [a,g]).

test_assert_includes_members(_Config) ->
    List = [a,b,c,d,e,f],

    % Should raise a members_missing error when members are not present
    try teal_lists:assert_includes_members(List, [a,g]) of
        _ -> erlang:error(failed)
    catch
        error:members_missing ->
            true
    end.

test_assert_includes_members_3(_Config) ->
    List = [a,b,c,d,e,f],
    Error = test123,

    % Should raise a the given error when members are not present
    try teal_lists:assert_includes_members(List, [a,g], Error) of
        _ -> erlang:error(failed)
    catch
        error:Error ->
            true
    end.

test_include(_Config) ->
    List = [a,b,c,d,e,f],

    % Should return true when item is present in list
    true = teal_lists:include(List, c),

    % Should return false when item is not present in list
    false = teal_lists:include(List, g).

test_assert_include(_Config) ->
    List = [a,b,c,d,e,f],

    % Should return true when item is present in list
    true = teal_lists:assert_include(List, c),

    % Should raise an error if item is not present
    try teal_lists:assert_include(List, g) of
        _ -> erlang:error(failed)
    catch
        error:member_missing ->
            true
    end.

test_order(_Config) ->
    Unordered = [3,4,5,1,2],
    Unordered2 = [b,a,c],
    Ordered = [1,2,3,4,5],
    Ordered2 = [a,b,c],
    Fun = fun(A,B) ->
            A =< B
    end,

    % Should return true when the order matches the order specified by the fun
    true = teal_lists:order(Ordered, Fun),
    true = teal_lists:order(Ordered2, Fun),

    % Should return false when the order does not match the order specified by the fun
    false = teal_lists:order(Unordered, Fun),
    false = teal_lists:order(Unordered2, Fun).

test_assert_order_2(_Config) ->
    Unordered = [3,4,5,1,2],
    Ordered = [1,2,3,4,5],
    Fun = fun(A,B) ->
            A =< B
    end,

    % Should return true when the order matches the order specified by the fun
    true = teal_lists:assert_order(Ordered, Fun),

    % Should raise an error if the order does not match the order specified by the fun
    try teal_lists:assert_order(Unordered, Fun) of
        _ -> erlang:error(failed)
    catch
        error:wrong_order ->
            true
    end.

test_assert_order_3(_Config) ->
    Error = msg,
    Unordered = [3,4,5,1,2],
    Ordered = [1,2,3,4,5],
    Fun = fun(A,B) ->
            A =< B
    end,

    % Should return true when the order matches the order specified by the fun
    true = teal_lists:assert_order(Ordered, Fun, Error),

    % Should raise an error if the order does not match the order specified by the fun
    try teal_lists:assert_order(Unordered, Fun, Error) of
        _ -> erlang:error(failed)
    catch
        error:Error ->
            true
    end.
