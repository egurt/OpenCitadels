-module(openCitadels_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(GAME, openCitadels_game).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([ start_link/0
        , setup/2
        , register/1
        , list_players/1
        , state/0
        , status/0
        , game_status/1
        , game_status/2
        , move/4
        , send/3
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ----
%% Record definitions
%% ----
-record(state, { games = []   %% #game
               , players = [] %% #player
               }).

-record(game, { id
              , pid
              , players = [] %% id
              }).
-record(player, { id
                , pid
                }).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

setup(ClientIDs, Options) ->
    gen_server:call(?SERVER, {setup, ClientIDs, Options}).

register(Pid) ->
    gen_server:call(?SERVER, {register, Pid}).

list_players(GameID) ->
    gen_server:call(?SERVER, {list_players, GameID}).

state() ->
    gen_server:call(?SERVER, state).

status() ->
    ok.

game_status(_GameID) ->
    ok.

game_status(_GameID, _Player) ->
    ok.

move(GameID, PlayerID, Move, Args) ->
    gen_server:cast(?SERVER, {move, GameID, PlayerID, Move, Args}).

send(GameID, PlayerID, Message) ->
    gen_server:cast(?SERVER, {send, GameID, PlayerID, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(no_args) ->
    {ok, #state{}}.


handle_call({register, Pid}, {Pid, _Tag} = _From, State) ->
    ID = erlang:now(),
    Player = #player{id = ID, pid = Pid},
    Players = [Player | State#state.players],
    {reply, {ok, ID}, State#state{players = Players}};

handle_call({setup, PlayerIDs, Options}, _From, State) ->
    Players = [  lists:keyfind(ID, #player.id, State#state.players)
              || ID <- PlayerIDs],
    case lists:member(false, Players) of
        true ->
            {reply, {error, 'unregistered player'}, State};
        false ->
            GameID = erlang:now(),
            %% use supervisor?
            {ok, GamePid} = ?GAME:start_link([ {id, GameID}
                                             , {players, PlayerIDs}
                                             | Options
                                             ]),
            NewGame = #game{ id = GameID 
                           , pid = GamePid
                           , players = PlayerIDs
                           },
            Games = [NewGame | State#state.games],
            {reply, {ok, GameID}, State#state{games = Games}}
    end;

handle_call({list_players, GameID}, _From, State) ->
    Reply = case gamefind(GameID, State) of
        false -> {error, 'wrong id'};
        Game  -> {ok, Game#game.players}
    end,
    {reply, Reply, State};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({move, GameID, PlayerID, Move, Data}, State) ->
    Pid = (gamefind(GameID, State))#game.pid,
    ?GAME:Move(Pid, PlayerID, Data),
    {noreply, State};

handle_cast({send, GameID, all, Message}, State) ->
    PlayerIDs = (gamefind(GameID, State))#game.players,
    sendplayers(PlayerIDs, {message, GameID, Message}, State),
    {noreply, State};

handle_cast({send, GameID, PlayerID, Message}, State) ->
    sendplayers([PlayerID], {message, GameID, Message}, State),
    {noreply, State};

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
gamefind(GameID, State) ->
    lists:keyfind(GameID, #game.id, State#state.games).

sendplayers(PlayerIDs, Message, State) ->
    Fun = fun (ID) ->
        Pid = (lists:keyfind(ID, #player.id, State#state.players))#player.pid,
        Pid ! Message
    end,
    lists:foreach(Fun, PlayerIDs).
