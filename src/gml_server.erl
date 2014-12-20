-module(gml_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {game}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([gen/3, game/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Game = gml_game:gen(100,100,50),
    State = #state{game = Game},
    {ok, State}.

handle_call({gen, W, H, C}, _From, State) ->
    Game = gml_game:gen(W,H,C),
    State1 = #state{game=Game},
    {reply, ok, State1};
handle_call(game, _From, State) ->
    {reply, State#state.game, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
