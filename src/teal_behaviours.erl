-module(teal_behaviours).

-export([has_callback/3,
    is_behaviour/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec has_callback(Module :: atom(), Name :: atom(), Arity :: integer()) ->
    boolean().

has_callback(Module, Name, Arity) ->
    Callbacks = get_callbacks(Module),
    lists:any(fun({callback, Details}) ->
                [CallbackDetails] = Details,
                {{CallbackName, CallbackArity}, _Args} = CallbackDetails,
                ({CallbackName, CallbackArity} == {Name, Arity})
        end, Callbacks).

-spec is_behaviour(Module :: atom()) -> boolean().

is_behaviour(Module) ->
    case get_callbacks(Module) of
        [] ->
            false;
        _ ->
            true
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================

get_callbacks(Module) ->
    Attributes = Module:module_info(attributes),
    lists:filter(fun({AttrName, _Opts}) ->
                case AttrName of
                    callback ->
                        true;
                    _ ->
                        false
                end
        end, Attributes).
