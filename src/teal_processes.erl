-module(teal_processes).

-export([is_registered/1, assert_is_registered/1, assert_is_registered/2,
        is_registered_with_name/2, assert_is_registered_with_name/2,
         assert_is_registered_with_name/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_registered(Process :: pid() | atom()) -> boolean().

is_registered(Process) when is_atom(Process) ->
    case erlang:whereis(Process) of
        undefined ->
            false;
        _ ->
            true
    end;

is_registered(Process) when is_pid(Process) ->
    case erlang:process_info(Process, [registered_name]) of
        [{registered_name, []}] ->
            false;
        _ ->
            true
    end.

-spec assert_is_registered(Process :: pid() | atom()) -> boolean().

assert_is_registered(Process) ->
    teal:assert(true, is_registered(Process), not_registered).

-spec assert_is_registered(Process :: pid() | atom(), Msg :: term()) -> boolean().

assert_is_registered(Process, Msg) ->
    teal:assert(true, is_registered(Process), Msg).

-spec is_registered_with_name(Process :: pid(), Name :: atom()) -> boolean().

is_registered_with_name(Process, Name) when is_pid(Process), is_atom(Name) ->
    case erlang:process_info(Process, [registered_name]) of
      [{registered_name, []}] ->
            false;
      [{registered_name, RegisteredName}] ->
            RegisteredName == Name
    end.

-spec assert_is_registered_with_name(Process :: pid(), Name :: atom()) ->
    boolean().

assert_is_registered_with_name(Process, Name) ->
    teal:assert(true, is_registered_with_name(Process, Name), wrong_name).

-spec assert_is_registered_with_name(Process :: pid(), Name :: atom(),
                                     Msg :: term()) -> boolean().

assert_is_registered_with_name(Process, Name, Msg) ->
    teal:assert(true, is_registered_with_name(Process, Name), Msg).

%%%===================================================================
%%% Private functions
%%%===================================================================
