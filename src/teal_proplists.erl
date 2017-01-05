-module(teal_proplists).

-export([sort_nested_proplist/1]).

sort_nested_proplist(Proplist = [{_,_}|_]) ->
    lists:keysort(1, [{Key, sort_nested_proplist(Value)} || {Key, Value} <- Proplist]);
sort_nested_proplist(List) when is_list(List) ->
    [sort_nested_proplist(Item) || Item <- List];
sort_nested_proplist(Other) ->
    Other.
