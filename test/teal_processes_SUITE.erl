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
         test_is_registered/1, test_assert_is_registered_1/1,
    test_assert_is_registered_2/1,
    test_get_state/1,
    test_receive_message/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_is_registered, test_assert_is_registered_1,
     test_assert_is_registered_2,
     test_get_state, test_should_receive].

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

test_assert_is_registered_1(_Config) ->
    [RegisteredAtom|_] = registered(),
    Registered = whereis(RegisteredAtom),

    % Should return true if the process is registered
    true = teal_processes:assert_is_registered(Registered),

    % Should raise an error if the process is not registered
    try teal_processes:assert_is_registered(me) of
        _ -> erlang:error(failed)
    catch
        error:not_registered ->
            true
    end.

test_assert_is_registered_2(_Config) ->
    [RegisteredAtom|_] = registered(),
    Registered = whereis(RegisteredAtom),
    Msg = failed,

    % Should return true if the process is registered
    true = teal_processes:assert_is_registered(Registered, Msg),

    % Should raise an error with a custom error message if not registered
    try teal_processes:assert_is_registered(me, Msg) of
        _ -> erlang:error(failed)
    catch
        error:failed ->
            true
    end.

test_get_state(_Config) ->
    % Should take a pid and return process state
    % We are assuming the sample_gen_server is available
    Name = sample_gen_server,
    Name:start_link(),
    GenServerPid = whereis(Name),
    ExpectedState = {state},
    ExpectedState = teal_processes:get_state(GenServerPid),

    % Should take a atom and return registered processes state
    ExpectedState = teal_processes:get_state(Name).

test_receive_message(_Config) ->
    Msg = test,
    WrongMsg = invalid_test,
    Timeout = 1000,
    Pid = self(),

    % Should return true when a matching message is received before timeout
    spawn_link(fun() -> Pid ! Msg end),
    true = teal_processes:should_receive(Msg, Timeout),

    % Should return false when a matching message is not received
    false = teal_processes:should_receive(Msg, Timeout),

    % Should return false when an incorrect message is received
    spawn_link(fun() -> Pid ! WrongMsg end),
    false = teal_processes:should_receive(Msg, Timeout).
