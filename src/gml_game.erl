%% Main module implementing Game of Life algorithm
-module(gml_game).

% API
-export([new/0, gen/3, view/5]).

% Types
-record(game, {state, generation}).

-opaque game() :: #game{}.
-export_type([game/0]).

%% API -------------------

-spec new() -> game().
new() ->
    #game{}.

-spec gen(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> game().
gen(W,H,C) ->
    #game{}.

-spec view(integer(), integer(), non_neg_integer(), non_neg_integer(), game()) -> string().
view(X,Y,W,H,G) ->
    "....#\n"
    "...#.\n".
