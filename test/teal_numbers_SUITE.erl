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
-export([test_close_to/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_close_to].

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
    true = teal_numbers:close_to(Input, AnotherTarget, Delta).
