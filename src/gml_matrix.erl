% Sparsed matrix ADT. Based on Dict structure.
% All cells are supposed to have 0 initialy.

-module(gml_matrix).

% API
-export([new/0,
         write/3, write/4, read/3,
         from_list/1, to_list/1,
         delete/2,
         sum/2, count/1,
         compact/1,view/5]).

% Types
-opaque matrix() :: dict().
-export_type([matrix/0]).

-type list_of_coords() :: [{integer(),integer()}].

%% API --------------------------

-spec new() -> matrix().
new() ->
    dict:new().

-spec write(integer(), integer(), matrix()) -> matrix().
write(X,Y,M) ->
    write(X,Y,1,M).

-spec write(integer(), integer(), integer(), matrix()) -> matrix().
write(X,Y,0,M) ->
    dict:erase({X,Y},M);
write(X,Y,Value,M) ->
    dict:store({X,Y},Value,M).

-spec read(integer(), integer(), matrix()) -> integer().
read(X,Y,M) ->
    case dict:find({X,Y},M) of
        {ok, Value} -> Value;
        error -> 0
    end.

-spec from_list(list_of_coords()) -> matrix().
from_list(L) ->
    lists:foldl(
      fun({X,Y}, M) -> write(X,Y,M) end,
      new(),
      L ).

-spec to_list(matrix()) -> list_of_coords().
to_list(M) ->
    [{X,Y} || {{X,Y},V} <- dict:to_list(M), V /= 0].

-spec delete(fun((integer()) -> boolean()), matrix()) -> matrix().
delete(Pred, M) ->
    dict:filter(fun(_K,V) -> not Pred(V) end, M).

-spec sum(matrix(),matrix()) -> matrix().
sum(M1,M2) ->
    dict:merge(fun(_K,V1,V2) -> V1 + V2 end, M1, M2).

-spec count(matrix()) -> non_neg_integer().
count(M) ->
    dict:size(compact(M)).

-spec compact(matrix()) -> matrix().
compact(M) ->
    delete(fun(V) -> V == 0 end,M).

-spec view(integer(), integer(), non_neg_integer(),non_neg_integer(), matrix()) -> [[(0|1)]].
view(X,Y,W,H,M) ->
    [ [ norm(read(CX,CY,M)) || CX <- lists:seq(X,X+W-1)]
     || CY <- lists:seq(Y, Y+H-1)].

%% Internal ------------------

norm(0) -> 0;
norm(_) -> 1.

%% Tests ---------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

write_read_test_() ->
    M = new(),
    M1 = write(1,2,M),
    M3 = write(1,2,3,M1),
    M4 = write(1,2,0,M3),
    [
     ?_assertMatch(0, read(1,2,M)),
     ?_assertMatch(1, read(1,2,M1)),
     ?_assertMatch(3, read(1,2,M3)),
     ?_assertMatch(0, read(1,2,M4))
    ].

from_to_list_test_() ->
    L = lists:sort([{1,2},{3,4},{5,6}]),
    M = from_list(L),
    [
     ?_assertMatch(1,read(1,2,M)),
     ?_assertMatch(1,read(3,4,M)),
     ?_assertMatch(1,read(5,6,M)),
     ?_assertMatch(L,lists:sort(to_list(M)))
    ].

delete_test_() ->
    M1 = from_list([{1,2},{3,4}]),
    M2 = write(5,6,10,M1),
    M3 = write(7,8,20,M2),
    M4 = delete(fun(V) -> V==10 end, M3),
    [
     ?_assertMatch(1, read(1,2,M4)),
     ?_assertMatch(1, read(3,4,M4)),
     ?_assertMatch(0, read(5,6,M4)),
     ?_assertMatch(20, read(7,8,M4))
    ].

sum_test_() ->
    M1 = from_list([{1,2},{3,4}]),
    M2 = from_list([{3,4},{5,6}]),
    M12 = sum(M1,M2),
    [
     ?_assertMatch(1, read(1,2,M12)),
     ?_assertMatch(2, read(3,4,M12)),
     ?_assertMatch(1, read(5,6,M12))
    ].


view_test_() ->
    M = gml_matrix:from_list([{0,1},{1,2},{2,0}]),
    [
     ?_assertMatch([[0,0,1],
                   [1,0,0],
                   [0,1,0]], view(0,0,3,3,M)),
     ?_assertMatch([[0,0,0],
                    [0,0,0]], view(100,100,3,2,M))
    ].

-endif.
