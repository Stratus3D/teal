-module(teal_numbers).

-export([close_to/3]).

%%%===================================================================
%%% API
%%%===================================================================

close_to(Received, Expected, Delta) ->
    Min = Expected - Delta,
    Max = Expected + Delta,

    case Received of
        Received when Received =< Max, Received >= Min ->
            true;
        _ ->
            false
    end.

%%%===================================================================
%%% Private functions
%%%===================================================================
