-module(teal_processes_SUITE).

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
-export([
         test_is_registered/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_is_registered].

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

test_is_registered(_Config) ->
    [RegisteredAtom|_] = registered(),
    Registered = whereis(RegisteredAtom),

    % Should return true if the process is registered
    true = teal_processes:is_registered(Registered),
    % Should return false if the process is not registered
    false = teal_processes:is_registered(self()),

    % Should also handle atoms
    true = teal_processes:is_registered(RegisteredAtom),
    false = teal_processes:is_registered(me).
