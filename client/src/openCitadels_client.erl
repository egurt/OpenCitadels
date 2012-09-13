-module(openCitadels_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, do/2, do/1, gdo/2, gdo/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, status/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(player, { id
               , games = []
               }).
-record(game, { id
              , actions = []
              }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

start_link(N) ->
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
    {ok, [#player{id = ID, games = [#game{id = GID}]}]};

init(N) when is_integer(N) ->
    Players = [element(2, openCitadels_server:register()) || _ <- lists:seq(1,N)],
    {ok, GID} = openCitadels_server:setup(Players, []),
    {ok, [#player{id = ID, games = [#game{id = GID}]} || ID <- Players]}.


handle_call(status, _From, Players) ->
    Game = hd((hd(Players))#player.games),
    Reply = openCitadels_server:game_status(Game#game.id),
    {reply, Reply, Players};

handle_call({do, N}, _From, [#player{id = PID, games = [Game]}] = State) ->
    Action = lists:nth(N, Game#game.actions),
    Reply = openCitadels_server:do(Game#game.id, PID, Action),
    {reply, Reply, State};

handle_call({gdo, GID, N}, _From, [#player{id = PID, games = Games}] = State) ->
    Action = lists:nth(N, (lists:keyfind(GID, #game.id, Games))#game.actions),
    Reply = openCitadels_server:do(GID, PID, Action),
    {reply, Reply, State};

handle_call({do, PID, N}, _From, Players) ->
    Game = hd((lists:keyfind(PID, #player.id, Players))#player.games),
    Action = lists:nth(N, Game#game.actions),
    Reply = openCitadels_server:do(Game#game.id, PID, Action),
    {reply, Reply, Players};

handle_call({gdo, GID, PID, N}, _From, Players) ->
    Games = (lists:keyfind(PID, #player.id, Players))#player.games,
    Action = lists:nth(N, (lists:keyfind(GID, #game.id, Games))#game.actions),
    Reply = openCitadels_server:do(GID, PID, Action),
    {reply, Reply, Players};

handle_call(Request, _From, State) ->
    io:format("Unmatched call: ~p\n", [Request]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({PID, {message, GID, {actions, Actions}}}, Players) ->
    Player = lists:keyfind(PID, #player.id, Players),
    Games = Player#player.games,
    Game = lists:keyfind(GID, #game.id, Games),
    NewPlayer = Player#player{games = lists:keyreplace(GID, #game.id, Games, Game#game{actions = Actions})},
    io:format("~w(~w) - Actions: ~p\n", [PID, GID, Actions]),
    {noreply, lists:keyreplace(PID, #player.id, Players, NewPlayer)};
    
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

