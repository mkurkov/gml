-module(gml_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {
          game :: gml_game:game(),
          mode :: run_mode(),
          watch = {} :: watch_mode()
         }).

-type run_mode() :: stop | run | {run, integer()}.
-type watch_mode() :: {} | {integer(), integer(), non_neg_integer(), non_neg_integer(), string()}.

-define(GAME_TICK, 100).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([set_game/1, game/0, run/1, run/0, pause/0, watch/5, unwatch/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec game() -> gml_game:game().
game() ->
    gen_server:call(?SERVER, game).

-spec run(integer()) -> ok.
run(N) ->
    gen_server:call(?SERVER, {run, N}).

-spec run() -> ok.
run() ->
    gen_server:call(?SERVER, run).

-spec pause() -> ok.
pause() ->
    gen_server:call(?SERVER, pause).

-spec set_game(gml_game:game()) -> ok.
set_game(Game) ->
    gen_server:call(?SERVER, {set_game, Game}).

-spec watch(integer(), integer(), non_neg_integer(), non_neg_integer(), string()) -> ok.
watch(X,Y,W,H,Message) ->
    gen_server:call(?SERVER, {watch, X, Y, W, H, Message}).

-spec unwatch() -> ok.
unwatch() ->
    gen_server:call(?SERVER, unwatch).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Game = gml_game:gen(100,100,50),
    State = #state{game = Game},
    {ok, State}.

handle_call({set_game, Game}, _From, State) ->
    State1 = State#state{game=Game},
    {reply, ok, State1};
handle_call(game, _From, State) ->
    {reply, State#state.game, State};
handle_call({run, N}, _From, State) ->
    send_tick(),
    S1 = State#state{mode={run, N}},
    {reply, ok, S1};
handle_call(run, _From, State) ->
    send_tick(),
    S1 = State#state{mode=run},
    {reply, ok, S1};
handle_call(pause, _From, State) ->
    S1 = State#state{mode=stopped},
    {reply, ok, S1};
handle_call({watch, X,Y,W,H,Message}, _From, State) ->
    S1 = State#state{watch = {X,Y,W,H,Message}},
    {reply, ok, S1};
handle_call(unwatch, _From, State) ->
    S1 = State#state{watch = {}},
    {reply, ok, S1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(step, #state{mode=Mode,game=Game} = State) ->
    State1 =
        case Mode of
            stopped ->
                State;
            {run, 0} ->
                State#state{mode=stopped};
            {run, N} ->
                G1 = gml_game:step(Game),
                send_tick(),
                State#state{mode={run, N-1}, game=G1};
            run ->
                G1 = gml_game:step(Game),
                send_tick(),
                State#state{game=G1}
        end,
    do_watch(State1),
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_tick() ->
    erlang:send_after(?GAME_TICK, self(), step).

do_watch(#state{watch={}}) ->
    ok;
do_watch(#state{game=G,watch={X,Y,W,H,Message}}) ->
    View = gml_game:view(X,Y,W,H,G),
    Generation = gml_game:generation(G),
    full_cls(),
    Footer = io_lib:format("Watching X:~p Y:~p W:~p H:~p. Generation: ~p.~n~s~n", [X,Y,W,H,Generation,Message]),
    io:format([View,Footer]).


%% move cursor to beginning of the line
first_row() ->
    io:format("\e[H").

%% clear the console
cls() ->
    io:format("\e[J").

%% both
full_cls() ->
    io:format("\e[H\e[J").
