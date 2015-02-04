-module(teal_numbers_SUITE).

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
-export([test_close_to/1,
        test_assert_close_to_2/1, test_assert_close_to_3/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_close_to, test_assert_close_to_2, test_assert_close_to_3].

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

test_close_to(_Config) ->
    Delta = 2,
    Target = 2,
    Input = 1,

    % Should return true when the input number is inside the range
    true = teal_numbers:close_to(Input, Target, Delta),

    % Should return false when the input number is outside of the range
    AnotherTarget = 10,
    false = teal_numbers:close_to(Input, AnotherTarget, Delta).

test_assert_close_to_2(_Config) ->
    Delta = 2,
    Target = 2,
    Input = 1,

    % Should return true when the input number is inside the range
    true = teal_numbers:assert_close_to(Input, Target, Delta),

    % Should raise an error when the input number is outside of the range
    AnotherTarget = 10,
    try teal_numbers:assert_close_to(Input, AnotherTarget, Delta) of
        _ -> erlang:error(failed)
    catch
        error:not_in_range ->
            true
    end.

test_assert_close_to_3(_Config) ->
    Delta = 2,
    Target = 2,
    Input = 1,
    Msg = test,

    % Should return true when the input number is inside the range
    true = teal_numbers:assert_close_to(Input, Target, Delta, Msg),

    % Should raise an error when the input number is outside of the range
    AnotherTarget = 10,
    try teal_numbers:assert_close_to(Input, AnotherTarget, Delta, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.
