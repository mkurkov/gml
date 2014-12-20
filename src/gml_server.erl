-module(gml_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {
          game :: gml_game:game(),
          mode :: run_mode()
         }).

-type run_mode() :: stop | run | {run, integer()}.

-define(GAME_TICK, 100).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([gen/3, game/0, run/1, run/0, pause/0]).

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

-spec gen(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ok.
gen(W,H,C) ->
    gen_server:call(?SERVER, {gen, W, H, C}).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Game = gml_game:gen(100,100,50),
    State = #state{game = Game},
    {ok, State}.

handle_call({gen, W, H, C}, _From, State) ->
    Game = gml_game:gen(W,H,C),
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
