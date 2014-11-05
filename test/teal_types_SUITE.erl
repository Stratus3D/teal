-module(teal_types_SUITE).

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
-export([test_not_record/1, test_assert_not_record_1/1,
         test_assert_is_module_2/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_not_record, test_assert_not_record_1, test_assert_is_module_2].

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

test_not_record(_Config) ->
    % Should return false when the term looks like a record
    false = teal_types:not_record({foo, bar}),

    % Should return true when the term cannot be a record
    true = teal_types:not_record({[], a}),
    true = teal_types:not_record(not_a_record).

test_assert_not_record_1(_Config) ->
    % Should return true when the term cannot be a record
    true = teal_types:assert_not_record({[], a}),
    true = teal_types:assert_not_record(not_a_record),

    % Should raise an error when the term looks like a record
    try teal_types:assert_not_record({foo, bar}) of
        _ -> erlang:error(failed)
    catch
        error:is_a_record ->
            true
    end.

test_assert_is_module_2(_Config) ->
    Msg = test,

    % Should return true when the term cannot be a record
    true = teal_types:assert_not_record({[], a}, Msg),
    true = teal_types:assert_not_record(not_a_record, Msg),

    % Should raise an error when the term looks like a record
    try teal_types:assert_not_record({foo, bar}, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.

