-module(gml_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Script API
-export([start/0]).

%% Application callbacks ------------

start(_StartType, _StartArgs) ->
    gml_sup:start_link().

stop(_State) ->
    ok.

%% Script API -----------------------

start() ->
    application:start(gml).
