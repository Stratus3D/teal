-module(teal).

-export([assert/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec assert(Lhs :: any(), Rhs :: any(), Message :: atom()) -> true.

assert(Lhs, Rhs, Message) ->
    try Lhs = Rhs of
        Lhs -> true
    catch
        error:{badmatch, _} ->
            erlang:error(Message)
    end.

-spec not_of_type(Term :: atom(), Type :: atom()) ->

not_of_type(Term, Type) ->
    not_implemented.

%%%===================================================================
%%% Private functions
%%%===================================================================
