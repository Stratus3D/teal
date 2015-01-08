-module(teal_numbers).

-export([close_to/3, assert_close_to/3, assert_close_to/4]).

%%%===================================================================
%%% API
%%%===================================================================

-spec close_to(Received :: number(), Expected :: number(),
               Delta :: number()) -> boolean().

close_to(Received, Expected, Delta) ->
    Min = Expected - Delta,
    Max = Expected + Delta,

    case Received of
        Received when Received =< Max, Received >= Min ->
            true;
        _ ->
            false
    end.

-spec assert_close_to(Received :: number(), Expected :: number(),
                      Delta :: number()) -> boolean().

assert_close_to(Received, Expected, Delta) ->
    teal:assert(true, close_to(Received, Expected, Delta), not_in_range).

-spec assert_close_to(Received :: number(), Expected :: number(),
                      Delta :: number(), Msg :: any()) -> boolean().

assert_close_to(Received, Expected, Delta, Msg) ->
    teal:assert(true, close_to(Received, Expected, Delta), Msg).

%%%===================================================================
%%% Private functions
%%%===================================================================
