-module(teal_behaviours_SUITE).

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
-export([test_has_callback/1, test_assert_has_callback_3/1, test_assert_has_callback_4/1,
         test_is_behaviour/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_has_callback, test_assert_has_callback_3, test_assert_has_callback_4,
     test_is_behaviour].

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

test_has_callback(_Config) ->
    % Should return true when the module is a behaviour
    true = teal_behaviours:has_callback(gen_server, handle_call, 3),

    % Should return false when the module is not a behaviour
    false = teal_behaviours:has_callback(erlang, callback, 1).

test_assert_has_callback_3(_Config) ->
    Msg = custom_msg,

    % Should return true when the module is a behaviour
    true = teal_behaviours:assert_has_callback(gen_server, handle_call, 3, Msg),

    % Should raise an error when the module is not a behaviour
    try teal_behaviours:assert_has_callback(erlang, callback, 1, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.

test_assert_has_callback_4(_Config) ->
    % Should return true when the module is a behaviour
    true = teal_behaviours:assert_has_callback(gen_server, handle_call, 3),

    % Should raise an error when the module is not a behaviour
    try teal_behaviours:assert_has_callback(erlang, callback, 1) of
        _ -> erlang:error(failed)
    catch
        error:no_callback ->
            true
    end.

test_is_behaviour(_Config) ->
    % Should return true when the module is a behaviour
    true = teal_behaviours:is_behaviour(gen_server),

    % Should return false when the module is not a behaviour
    false = teal_behaviours:is_behaviour(erlang).
