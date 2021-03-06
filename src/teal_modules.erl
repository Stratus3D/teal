-module(teal_modules).

-export([is_module/1, assert_is_module/1, assert_is_module/2,
         exports/2, assert_exports/2, assert_exports/3,
         exports_with_arity/3, assert_exports_with_arity/3,
         assert_exports_with_arity/4]).

%%%===================================================================
%%% API
%%%===================================================================

-spec is_module(Atom :: atom()) -> boolean().

is_module(Atom) ->
    try Atom:module_info() of
        _ ->
            true
    catch
        error:_ ->
            false
    end.

-spec assert_is_module(Atom :: atom()) -> boolean().

assert_is_module(Atom) ->
    teal:assert(true, is_module(Atom), not_a_module).

-spec assert_is_module(Atom :: atom(), Msg :: atom()) -> boolean().

assert_is_module(Atom, Msg) ->
    teal:assert(true, is_module(Atom), Msg).

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

-spec assert_exports(Module :: atom(), Function :: atom()) ->
    boolean().

assert_exports(Module, Function) ->
    teal:assert(true, exports(Module, Function), function_not_exported).

-spec assert_exports(Module :: atom(), Function :: atom(), Msg :: atom()) ->
    boolean().

assert_exports(Module, Function, Msg) ->
    teal:assert(true, exports(Module, Function), Msg).

-spec exports_with_arity(Module :: atom(), Function :: atom(), Arity :: integer()) ->
    boolean().

exports_with_arity(Module, Function, Arity) ->
    Exports = module_exports(Module),
    lists:any(fun(Export) ->
                case Export of
                    {Function, Arity} ->
                        true;
                    _ ->
                        false
                end
        end, Exports).

-spec assert_exports_with_arity(Module :: atom(), Function :: atom(),
                                Arity :: integer()) -> boolean().

assert_exports_with_arity(Module, Function, Arity) ->
    teal:assert(true, exports_with_arity(Module, Function, Arity),
                function_not_exported).

-spec assert_exports_with_arity(Module :: atom(), Function :: atom(),
                                Arity :: integer(), Msg :: atom()) -> boolean().

assert_exports_with_arity(Module, Function, Arity, Msg) ->
    teal:assert(true, exports_with_arity(Module, Function, Arity), Msg).

%%%===================================================================
%%% Private functions
%%%===================================================================

module_exports(Module) ->
    Module:module_info(exports).
