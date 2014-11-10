-module(teal_processes).

-export([is_registered/1]).


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
