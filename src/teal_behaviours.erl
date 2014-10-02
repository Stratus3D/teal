-module(teal_behaviours).

-export([is_behaviour/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_behaviour(Module :: atom()) -> boolean().

is_behaviour(Module) ->
    case get_behaviours(Module) of
        [] ->
            false;
        _ ->
            true
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================

get_behaviours(Module) ->
    Attributes = Module:module_info(attributes),
    lists:filter(fun({AttrName, _Opts}) ->
                case AttrName of
                    callback ->
                        true;
                    _ ->
                        false
                end
        end, Attributes).
