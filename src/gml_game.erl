%% Main module implementing Game of Life algorithm
-module(gml_game).

% API
-export([new/0, gen/3, view/5, generation/1, step/1, load/1, save/6]).

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

-spec generation(game()) -> non_neg_integer().
generation(#game{generation=G}) ->
    G.

-spec step(game()) -> game().
step(#game{state=M, generation=C}) ->
    %% add 1 point to every cell near live one and 100 to alive themself
    ME = expand_matrix(M),
    %% keep all cells with 3 points (new born), 102 and 103 - old with enough neighbours
    MNew = gml_matrix:delete(fun(V) -> V /= 3 andalso V /= 102 andalso V /= 103 end, ME),
    #game{state = MNew, generation=C+1}.

-spec load(string()) -> game().
load(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {ok,M} = load_lines(File),
    file:close(File),
    #game{state=M, generation=0}.

-spec save(integer(),integer(),non_neg_integer(),non_neg_integer(), string(), game()) -> ok.
save(X,Y,W,H, FileName, Game) ->
    Header = io_lib:format("! GML Game of life dump file~n"
                           "! Generation: ~p~n"
                           "! Viewport: X:~p Y:~p W:~p H:~p~n",
                           [Game#game.generation,X,Y,W,H]),
    View = view(X,Y,W,H,Game),
    file:write_file(FileName,[Header,View]).

%% Internal --------------

show_cell(0,Dead,_Live) ->
    Dead;
show_cell(_,_Dead,Live) ->
    Live.

expand_matrix(MOld) ->
    gml_matrix:fold_live(
      fun(X,Y,M) ->
              gml_matrix:sum(M,expand_cell(X,Y))
      end,
      gml_matrix:new(),
      MOld).

expand_cell(X,Y) ->
    Cells = [{X+Xd, Y+Yd} || Xd <- [-1,0,1], Yd <- [-1,0,1],  {Xd,Yd} /= {0,0} ],
    M = gml_matrix:from_list(Cells),
    % max cell count can be 8, so we add some big constant to distinct new and old cells
    gml_matrix:write(X,Y,100,M).


load_lines(File) ->
    load_lines(File,0,gml_matrix:new()).
load_lines(File, Y, M) ->
    case file:read_line(File) of
        {ok, "!" ++ _Line} ->
            load_lines(File, Y, M);
        {ok, Line} ->
            M1 = load_chars(Line,0,Y,M),
            load_lines(File, Y+1, M1);
        eof ->
            {ok, M}
    end.

load_chars("\n", _X, _Y, M) ->
    M;
load_chars("", _X, _Y, M) ->
    M;
load_chars([C | Line], X, Y, M)  when C == $. orelse C == 32 ->
    load_chars(Line,X+1,Y,M);
load_chars([_ | Line], X, Y, M) ->
    load_chars(Line, X+ 1, Y, gml_matrix:write(X,Y,M)).


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

step_test_() ->
    Osc = gml_matrix:from_list([{1,0},{1,1},{1,2}]),
    G = #game{state = Osc, generation = 0},
    G1 = step(G),
    G2 = step(G1),
    [
     ?_assertMatch("...\n"
                   "###\n"
                   "...\n", view(0,0,3,3,G1)),
     ?_assertMatch(1, G1#game.generation),
     ?_assertMatch(".#.\n"
                   ".#.\n"
                   ".#.\n", view(0,0,3,3,G2)),
     ?_assertMatch(2, G2#game.generation)
    ].

load_test_() ->
    io:format("HI"),
    G = gml_game:load("../priv/oscillator.gml"),
    [
     ?_assertMatch("...\n"
                   "###\n"
                   "...\n", view(0,0,3,3,G)),
     ?_assertMatch(0, G#game.generation)
    ].

-endif.
