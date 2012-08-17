-module(openCitadels_client).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, do/2, do/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state, { id
               , games = []
               }).
-record(game, { id
              , actions = []
              }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

do(GID, N) ->
    gen_server:call(?SERVER, {do, GID, N}).
do(N) ->
    gen_server:call(?SERVER, {do, N}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(no_args) ->
    {ok, ID} = openCitadels_server:register(),
    {ok, GID} = openCitadels_server:setup([ID], []),
    {ok, #state{id = ID, games = [#game{id = GID}]}}.

handle_call({do, N}, _From, #state{games = [Game]} = State) ->
    Action = lists:nth(N, Game#game.actions),
    Reply = openCitadels_server:do(Game#game.id, State#state.id, Action),
    {reply, Reply, State};

handle_call({do, GID, N}, _From, State) ->
    Action = lists:nth(N, (lists:keyfind(GID, #game.id, State#state.games))#game.actions),
    Reply = openCitadels_server:do(GID, State#state.id, Action),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    io:format("Unmatched call: ~p\n", [Request]),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({message, GID, {actions, Actions}}, State) ->
    io:format("~w - Actions: ~p\n", [GID, Actions]),
    {noreply, State#state{games = lists:keyreplace(GID, #game.id, State#state.games, (lists:keyfind(GID, #game.id, State#state.games))#game{actions = Actions})}};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

