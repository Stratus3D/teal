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
-export([test_not_equal/1, test_assert_not_equal_2/1, test_assert_not_equal_3/1,
         test_assert/1,
         test_raises_exception/1, test_assert_raises_exception_1/1,
         test_assert_raises_exception_2/1,
         test_raises_exception_with_message/1,
         test_assert_raises_exception_with_message_1/1,
         test_assert_raises_exception_with_message_2/1,
         test_raises_throw/1,
         test_assert_raises_throw_1/1,
         test_assert_raises_throw_2/1,
         test_raises_throw_with_message/1,
         test_assert_raises_throw_with_message_1/1,
         test_assert_raises_throw_with_message_2/1,
         test_raises_error/1,
         test_raises_exit/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_not_equal, test_assert_not_equal_2, test_assert_not_equal_3,
     test_assert,
     test_raises_exception, test_assert_raises_exception_1,
     test_assert_raises_exception_2,
     test_raises_exception_with_message,
     test_assert_raises_exception_with_message_1,
     test_assert_raises_exception_with_message_2,
     test_raises_throw,
     test_assert_raises_throw_1,
     test_assert_raises_throw_2,
     test_raises_throw_with_message,
     test_assert_raises_throw_with_message_1,
     test_assert_raises_throw_with_message_2,
     test_raises_error,
     test_raises_exit].

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

test_not_equal(_Config) ->
    % Should return true when terms are not equal
    true = teal:not_equal(a, b),

    % Should return false when terms are equal
    false = teal:not_equal(a, a).

test_assert_not_equal_2(_Config) ->
    % Should return true when terms are not equal
    true = teal:assert_not_equal(a, b),

    % Should raise an error when the terms are equal
    try teal:assert_not_equal(a, a) of
        _ -> erlang:error(failed)
    catch
        error:equal ->
            true
    end.

test_assert_not_equal_3(_Config) ->
    Msg = test,

    % Should return true when terms are not equal
    true = teal:assert_not_equal(a, b, Msg),

    % Should raise an error when the terms are equal
    try teal:assert_not_equal(a, a, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.

test_raises_exception(_Config) ->
    % Should return true when the fun raises an exception
    true = teal:raises_exception(fun() -> throw(something) end),
    true = teal:raises_exception(fun() -> error(something) end),
    true = teal:raises_exception(fun() -> exit(something) end),

    % Should return false when the fun doesn't raises an exception
    false = teal:raises_exception(fun() -> true end).

test_assert_raises_exception_1(_Config) ->
    % Should return true when the fun raises an exception
    true = teal:assert_raises_exception(fun() -> throw(something) end),
    true = teal:assert_raises_exception(fun() -> error(something) end),
    true = teal:assert_raises_exception(fun() -> exit(something) end),

    % Should raise an error when the fun doesn't raises an exception
    true = teal:raises_exception(fun() ->
                    teal:assert_raises_exception(fun() -> true end)
            end).

test_assert_raises_exception_2(_Config) ->
    Msg = test,

    % Should return true when the fun raises an exception
    true = teal:assert_raises_exception(fun() -> throw(something) end, Msg),
    true = teal:assert_raises_exception(fun() -> error(something) end, Msg),
    true = teal:assert_raises_exception(fun() -> exit(something) end, Msg),

    % Should raise an error when the fun doesn't raises an exception
    true = teal:raises_exception(fun() ->
                    teal:assert_raises_exception(fun() -> true end, Msg)
            end).

test_raises_exception_with_message(_Config) ->
    Msg = something,

    % Should return true when the fun raises an exception
    true = teal:raises_exception_with_message(fun() -> throw(Msg) end, Msg),
    true = teal:raises_exception_with_message(fun() -> error(Msg) end, Msg),
    true = teal:raises_exception_with_message(fun() -> exit(Msg) end, Msg),

    % Should return false when the fun doesn't raises an exception
    false = teal:raises_exception_with_message(fun() -> true end, Msg).

test_assert_raises_exception_with_message_1(_Config) ->
    Msg = something,

    % Should return true when the fun raises an exception
    true = teal:assert_raises_exception_with_message(fun() ->
                    throw(Msg) end, Msg),
    true = teal:assert_raises_exception_with_message(fun() ->
                    error(Msg) end, Msg),
    true = teal:assert_raises_exception_with_message(fun() ->
                    exit(Msg) end, Msg),

    % Should raise an error when the fun doesn't raises an exception
    true = teal:raises_exception(fun() ->
                    teal:assert_raises_exception_with_message(fun() ->
                                true end, Msg)
            end).

test_assert_raises_exception_with_message_2(_Config) ->
    ErrMsg = something,
    Msg = test,

    % Should return true when the fun raises an exception
    true = teal:assert_raises_exception_with_message(fun() ->
                    throw(ErrMsg) end, ErrMsg, Msg),
    true = teal:assert_raises_exception_with_message(fun() ->
                    error(ErrMsg) end, ErrMsg, Msg),
    true = teal:assert_raises_exception_with_message(fun() ->
                    exit(ErrMsg) end, ErrMsg, Msg),

    % Should raise an error when the fun doesn't raises an exception
    teal:assert_raises_exception(fun() ->
                    teal:assert_raises_exception_with_message(fun() ->
                            true end, ErrMsg, Msg)
            end).

test_raises_throw(_Config) ->
    % Should return true when the fun raises a throw
    true = teal:raises_throw(fun() -> throw(something) end),

    % Should return false when the fun doesn't raises a throw
    false = teal:raises_throw(fun() -> true end).

test_assert_raises_throw_1(_Config) ->
    % Should return true when the fun raises a throw
    true = teal:assert_raises_throw(fun() -> throw(something) end),

    % Should raise an error when the fun doesn't raises an throw
    teal:assert_raises_exception(fun() ->
                    teal:assert_raises_throw(fun() -> true end)
            end).

test_assert_raises_throw_2(_Config) ->
    Msg = test,

    % Should return true when the fun raises a throw
    true = teal:assert_raises_throw(fun() -> throw(something) end, Msg),

    % Should raise an error when the fun doesn't raises an throw
    teal:assert_raises_exception(fun() ->
                    teal:assert_raises_throw(fun() -> true end, Msg)
            end).

test_raises_throw_with_message(_Config) ->
    ErrMsg = something,

    % Should return true when the fun raises a throw
    true = teal:raises_throw_with_message(fun() -> throw(something) end, ErrMsg),

    % Should return false when the fun doesn't raises a throw
    false = teal:raises_throw_with_message(fun() -> true end, ErrMsg).

test_assert_raises_throw_with_message_1(_Config) ->
    ErrMsg = something,

    % Should return true when the fun raises a throw with a matching message
    true = teal:assert_raises_throw_with_message(fun() ->
                    throw(ErrMsg) end, ErrMsg),

    % Should raise an error when the fun doesn't raises an throw with a matching
    % message
    teal:assert_raises_exception(fun() ->
                    teal:assert_raises_throw_with_message(fun() ->
                            true end, ErrMsg)
            end).

test_assert_raises_throw_with_message_2(_Config) ->
    ErrMsg = something,
    Msg = test,

    % Should return true when the fun raises a throw
    true = teal:assert_raises_throw_with_message(fun() ->
                    throw(ErrMsg) end, ErrMsg, Msg),

    % Should raise an error when the fun doesn't raises an throw
    teal:assert_raises_exception(fun() ->
                    teal:assert_raises_throw_with_message(fun() ->
                            true end, ErrMsg, Msg)
            end).


test_raises_error(_Config) ->
    % Should return true when the fun raises an error
    true = teal:raises_error(fun() -> error(something) end),

    % Should return false when the fun doesn't raises an error
    false = teal:raises_error(fun() -> true end).


test_raises_exit(_Config) ->
    % Should return true when the fun raises an exit
    true = teal:raises_exit(fun() -> exit(something) end),

    % Should return false when the fun doesn't raises an exit
    false = teal:raises_exit(fun() -> true end).
