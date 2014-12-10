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
-export([test_not_of_type/1,
         test_not_record/1, test_assert_not_record_1/1,
         test_assert_not_record_2/1,
         test_could_be_record/1, test_assert_could_be_record_1/1,
         test_assert_could_be_record_2/1]).


-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_not_of_type,
     test_not_record, test_assert_not_record_1, test_assert_not_record_2,
     test_could_be_record, test_assert_could_be_record_1,
     test_assert_could_be_record_2
    ].

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

test_not_of_type(_Config) ->
    % Should return false when the types match
    false = teal_types:not_of_type(a, atom),
    false = teal_types:not_of_type(<<"test">>, binary),
    false = teal_types:not_of_type(<<"test">>, bitstring),
    false = teal_types:not_of_type(true, boolean),
    false = teal_types:not_of_type(1.0, float),
    false = teal_types:not_of_type(fun() -> ok end, function),
    false = teal_types:not_of_type(1, integer),
    false = teal_types:not_of_type([], list),
    false = teal_types:not_of_type(1, number),
    false = teal_types:not_of_type(self(), pid),
    false = teal_types:not_of_type({}, tuple),
    %false = teal_types:not_of_type(port, port),
    %false = teal_types:not_of_type(reference, reference),

    % Should return true when types are different
    true = teal_types:not_of_type(a, binary).

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

test_assert_not_record_2(_Config) ->
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

test_could_be_record(_Config) ->
    % Should return true when a term looks like a record
    true = teal_types:could_be_record({foo, bar}),

    % Should return false when the term cannot be a record
    false = teal_types:could_be_record({[], a}),
    false = teal_types:could_be_record(not_a_record).

test_assert_could_be_record_1(_Config) ->
    % Should return true when a term looks like a record
    true = teal_types:assert_could_be_record({foo, bar}),

    % Should raise an error when the term cannot be a record
    try teal_types:assert_could_be_record({[], a}) of
        _ -> erlang:error(failed)
    catch
        error:not_record ->
            true
    end,
    try teal_types:assert_could_be_record(not_a_record) of
        _ -> erlang:error(failed)
    catch
        error:not_record ->
            true
    end.

test_assert_could_be_record_2(_Config) ->
    Msg = test,

    % Should return true when a term looks like a record
    true = teal_types:assert_could_be_record({foo, bar}),

    % Should raise an error when the term cannot be a record
    try teal_types:assert_could_be_record({[], a}, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end,
    try teal_types:assert_could_be_record(not_a_record, Msg) of
        _ -> erlang:error(failed)
    catch
        error:Msg ->
            true
    end.
