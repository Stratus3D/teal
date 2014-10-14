-module(teal_behaviours).

-export([
    has_callback/3, assert_has_callback/3, assert_has_callback/4,
    is_behaviour/1, assert_is_behaviour/1, assert_is_behaviour/2,
    implements_behaviour/2]).

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

-spec assert_has_callback(Module :: atom(), Name :: atom(),
                          Arity :: integer()) -> boolean().

assert_has_callback(Module, Name, Arity) ->
    teal:assert(true, has_callback(Module, Name, Arity), no_callback).

-spec assert_has_callback(Module :: atom(), Name :: atom(),
                          Arity :: integer(), Msg :: atom()) -> boolean().

assert_has_callback(Module, Name, Arity, Msg) ->
    teal:assert(true, has_callback(Module, Name, Arity), Msg).


-spec is_behaviour(Module :: atom()) -> boolean().

is_behaviour(Module) ->
    case get_callbacks(Module) of
        [] ->
            false;
        _ ->
            true
    end.

-spec assert_is_behaviour(Module :: atom()) -> boolean().

assert_is_behaviour(Module) ->
    teal:assert(true, is_behaviour(Module), not_behaviour).

-spec assert_is_behaviour(Module :: atom(), Msg :: atom()) -> boolean().

assert_is_behaviour(Module, Msg) ->
    teal:assert(true, is_behaviour(Module), Msg).


-spec implements_behaviour(Module :: atom(), Behaviour :: atom()) -> boolean().

implements_behaviour(Module, Behaviour) ->
    Callbacks = get_callbacks(Behaviour),
    Exports = Module:module_info(exports),
    CallbackNameArities = callbacks_to_name_arity(Callbacks),
    lists:all(fun(Callback) ->
                lists:member(Callback, Exports)
        end, CallbackNameArities).

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

callbacks_to_name_arity(Callbacks) ->
    lists:map(fun({_, [{{Name, Arity}, _}]}) ->
                       {Name, Arity}
                       end, Callbacks).
