-module(teal_modules_SUITE).

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
-export([test_is_module_1/1,
         test_assert_is_module_1/1,
         test_assert_is_module_2/1,
         test_exports_2/1,
         test_assert_exports_2/1,
         test_assert_exports_3/1,
         test_exports_with_arity_3/1,
         test_assert_exports_with_arity_3/1,
         test_assert_exports_with_arity_4/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_is_module_1, test_assert_is_module_1, test_assert_is_module_2,
     test_exports_2, test_assert_exports_2, test_assert_exports_3,
     test_exports_with_arity_3, test_assert_exports_with_arity_3,
     test_assert_exports_with_arity_4].


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

test_is_module_1(_Config) ->
    % Should return true when the atom is a valid module
    true = teal_modules:is_module(erlang),

    % Should return false when no module name matches the atom passed in
    false = teal_modules:is_module(abc).

test_assert_is_module_1(_Config) ->
    % Should return true when the atom is a valid module
    true = teal_modules:assert_is_module(erlang),

    % Should raise an error when no module name matches the atom passed in
    try teal_modules:assert_is_module(abc) of
        _ -> erlang:error(failed)
    catch
        error:not_a_module ->
            true
    end.

test_assert_is_module_2(_Config) ->
    Msg = test,

    % Should return true when the atom is a valid module
    true = teal_modules:assert_is_module(erlang, Msg),

    % Should raise an error when no module name matches the atom passed in
    try teal_modules:assert_is_module(abc, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.

test_exports_2(_Config) ->
    % Should return true when the module exports a function with the given name
    true = teal_modules:exports(erlang, port_call),

    % Should return false when the module does not export a function with the
    % name
    false = teal_modules:exports(erlang, missing_fun).

test_assert_exports_2(_Config) ->
    % Should return true when the module exports a function with the given name
    true = teal_modules:assert_exports(erlang, port_call),

    % Should raise an error when the module does not export a function with the
    % name
    try teal_modules:assert_exports(erlang, missing_fun) of
        _ -> erlang:error(failed)
    catch
        error:function_not_exported ->
            true
    end.

test_assert_exports_3(_Config) ->
    CustomMsg = custom_msg,
    % Should return true when the module exports a function with the given name
    true = teal_modules:assert_exports(erlang, port_call),

    % Should raise an error when the module does not export a function with the
    % name
    try teal_modules:assert_exports(erlang, missing_fun, CustomMsg) of
        _ -> erlang:error(failed)
    catch
        error:CustomMsg ->
            true
    end.

test_exports_with_arity_3(_Config) ->
    % Should return true when the module exports a function with the given name
    % and arity
    true = teal_modules:exports_with_arity(erlang, port_call, 2),

    % Should return false when the module does not export a function with the
    % name and arity
    false = teal_modules:exports_with_arity(erlang, missing_fun, 2),

    % Should return false when the module does not export a function with the
    % same arity
    false = teal_modules:exports_with_arity(erlang, port_call, 1).

test_assert_exports_with_arity_3(_Config) ->
    % Should return true when the module exports a function with the given name
    % and arity
    true = teal_modules:assert_exports_with_arity(erlang, port_call, 2),

    % Should raise an error when the module does not export a function with the
    % name and arity given
    try teal_modules:assert_exports_with_arity(erlang, missing_fun, 2) of
        _ -> erlang:error(failed)
    catch
        error:function_not_exported ->
            true
    end.

test_assert_exports_with_arity_4(_Config) ->
    CustomMsg = custom_msg,
    % Should return true when the module exports a function with the given name
    % and arity
    true = teal_modules:assert_exports_with_arity(erlang, port_call, 2),

    % Should raise an error with the custom message when the module does not
    % export a function with the name and arity given
    try teal_modules:assert_exports_with_arity(erlang, missing_fun, 2,
                                               CustomMsg) of
        _ -> erlang:error(failed)
    catch
        error:CustomMsg ->
            true
    end.
