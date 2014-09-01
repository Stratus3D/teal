-module(teal_SUITE).

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
-export([test_assert/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_assert].

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

test_assert(_Config) ->
    % Should return true when first to arguments are equal
    true = teal:assert(1, 1, broken),

    % Should return true when first to arguments are equal tuples
    Tuple = {a, b, c},
    true = teal:assert(Tuple, Tuple, broken),

    % Should raise error when first to arguments are not equal
    try teal:assert(1, 2, broken) of
        _ -> erlang:error(test_failure)
    catch
        error:broken ->
            true
    end,

    % Should return true when first to arguments are equal tuples
    Tuple = {a, b, c},
    Tuple2 = {d, e, f},
    try teal:assert(Tuple, Tuple2, broken) of
        _ -> erlang:error(test_failure)
    catch
        error:broken ->
            true
    end.
