-module(openCitadels_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_normal, _no_args) ->
    openCitadels_server_sup:start_link().

stop(_State) ->
    ok.
