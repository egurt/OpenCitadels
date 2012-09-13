-module(openCitadels_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, start/1, do/2, do/1, gdo/2, gdo/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, status/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(game, { id
              , actions = []
              , players = []
              , current_player
              }).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

start(N) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, N, []).

do(N) ->
    gen_server:call(?SERVER, {do, N}).

do(PID, N) ->
    gen_server:call(?SERVER, {do, PID, N}).

gdo(GID, N) ->
    gen_server:call(?SERVER, {gdo, GID, N}).

gdo(GID, PID, N) ->
    gen_server:call(?SERVER, {gdo, GID, PID, N}).

status() ->
    gen_server:call(?SERVER, status).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(no_args) ->
    {ok, ID} = openCitadels_server:register(),
    {ok, GID} = openCitadels_server:setup([ID], []),
    {ok, [#game{id = GID, players = [ID], current_player = ID}]};

init(N) when is_integer(N) ->
    Players = [element(2, openCitadels_server:register()) || _ <- lists:seq(1,N)],
    {ok, GID} = openCitadels_server:setup(Players, []),
    {ok, [#game{id = GID, current_player = hd(Players)}]}.


handle_call(status, _From, [Game | _] = State) ->
    Reply = openCitadels_server:game_status(Game#game.id),
    {reply, Reply, State};

handle_call({do, N}, _From, [Game | _] = State) ->
    #game{ current_player = PID
         , actions = Actions
         , id = ID
         } = Game,
    Action = lists:nth(N, Actions),
    Reply = openCitadels_server:do(ID, PID, Action),
    {reply, Reply, State};

handle_call({gdo, GID, N}, _From, State) ->
    #game{ current_player = PID
         , actions = Actions
         } = lists:keyfind(GID, #game.id, State),
    Action = lists:nth(N, Actions),
    Reply = openCitadels_server:do(GID, PID, Action),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    io:format("Unmatched call: ~p\n", [Request]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({PID, {message, GID, {actions, Actions}}}, State) ->
    Game = lists:keyfind(GID, #game.id, State),
    NewGame = Game#game{ current_player = PID
                       , actions = Actions
                       },
    io:format("~w(~w) - Actions: ~p\n", [PID, GID, Actions]),
    {noreply, lists:keyreplace(GID, #game.id, State, NewGame)};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

