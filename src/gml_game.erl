%% Main module implementing Game of Life algorithm
-module(gml_game).

% API
-export([new/0, gen/3, view/5]).

% Types
-record(game, {state :: gml_matrix:matrix(),
               generation :: integer()}).

-opaque game() :: #game{}.
-export_type([game/0]).

%% API -------------------

-spec new() -> game().
new() ->
    #game{state=gml_matrix:new(), generation=0}.

-spec gen(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> game().
gen(W,H,C) ->
    gen(W,H,C,gml_matrix:new()).

gen(_W,_H,0,M) ->
    #game{state = M, generation=0};
gen(W,H,N,M) ->
    X = random:uniform(W),
    Y = random:uniform(H),
    M1 = gml_matrix:write(X,Y,M),
    gen(W,H,N-1,M1).

-spec view(integer(), integer(), non_neg_integer(), non_neg_integer(), game()) -> string().
view(X,Y,W,H,#game{state=M}) ->
    View = gml_matrix:view(X,Y,W,H,M),
    Lines = [[show_cell(V,$.,$#) || V <- Row] || Row <- View],
    string:join(Lines, "\n") ++ "\n".



%% Internal --------------

show_cell(0,Dead,_Live) ->
    Dead;
show_cell(_,_Dead,Live) ->
    Live.

%% Tests -----------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gen_test_() ->
    G = gen(100,100,50),
    [
     ?_assert(gml_matrix:count(G#game.state) > 0),
     ?_assertMatch(0, G#game.generation)
    ].


view_test_() ->
    G = gen(3,3,1),
    ?_assertMatch("...\n...\n", view(10,10,3,2,G)).

-endif.
