%% Main external API endpoint

-module(gml).

% API
-export([gen/3,run/0,run/1,pause/0,view/4,load/1,save/5]).

%% API

-spec gen(non_neg_integer(),non_neg_integer(),non_neg_integer()) -> ok.
gen(W,H,C) when is_integer(W), is_integer(H), is_integer(C),
                W > 0, H > 0, C > 0
->
    gml_server:gen(W,H,C).

-spec view(integer(),integer(),non_neg_integer(),non_neg_integer()) -> ok.
view(X,Y,W,H) when is_integer(X), is_integer(Y), is_integer(W), is_integer(H),
                   W > 0, H > 0
->
    Game = gml_server:game(),
    View = gml_game:view(X,Y,W,H,Game),
    io:format(View).

-spec run() -> ok.
run() ->
    ok.

-spec run(non_neg_integer()) -> ok.
run(N) when is_integer(N), N > 0 ->
    ok.

-spec pause() -> ok.
pause() ->
    ok.


-spec load(string()) -> ok.
load(FileName) ->
    ok.

-spec save(integer(), integer(), non_neg_integer(), non_neg_integer(), string()) -> ok.
save(X,Y,W,H,FileName) when is_integer(X), is_integer(Y), is_integer(W), is_integer(H),
                            W > 0, H > 0
->
    ok.
