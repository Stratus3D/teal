-module(teal_modules).

-export([exports/2, exports/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec exports(Module :: atom(), Function :: atom()) ->
    boolean().

exports(Module, Function) ->
    Exports = module_exports(Module),
    lists:any(fun(Export) ->
                case Export of
                    {Function, _} ->
                        true;
                    _ ->
                        false
                end
        end, Exports).

-spec exports(Module :: atom(), Function :: atom(), Arity :: integer()) ->
    boolean().

exports(Module, Function, Arity) ->
    Exports = module_exports(Module),
    lists:any(fun(Export) ->
                case Export of
                    {Function, Arity} ->
                        true;
                    _ ->
                        false
                end
        end, Exports).

%%%===================================================================
%%% Private functions
%%%===================================================================

module_exports(Module) ->
    Module:module_info(exports).
