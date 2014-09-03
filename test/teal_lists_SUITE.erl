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
-export([test_is_flat/1,
         test_assert_is_flat/1,
         test_includes_members/1,
         test_assert_includes_members/1,
         test_include/1,
         test_assert_include/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_is_flat, test_assert_is_flat,
     test_includes_members, test_assert_includes_members,
     test_include, test_assert_include].

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

    try teal_lists:assert_is_flat(List) of
        _ -> erlang:error(failed)
    catch
        error:contains_sublists ->
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

    try teal_lists:assert_includes_members(List, [a,g]) of
        _ -> erlang:error(failed)
    catch
        error:members_missing ->
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
