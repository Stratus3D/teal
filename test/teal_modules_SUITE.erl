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
-export([test_exports_2/1,
         test_exports_3/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_exports_2, test_exports_3].

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

test_exports_2(_Config) ->
    % Should return true when the module exports a function with the given name
    true = teal_modules:exports(erlang, port_call),

    % Should return false when the module does not export a function with the
    % name
    false = teal_modules:exports(erlang, missing_fun).

test_exports_3(_Config) ->
    % Should return true when the module exports a function with the given name
    true = teal_modules:exports(erlang, port_call, 2),

    % Should return false when the module does not export a function with the
    % name
    false = teal_modules:exports(erlang, missing_fun, 2),

    % Should return false when the module does not export a function with the
    % same arity
    false = teal_modules:exports(erlang, port_call, 1).
