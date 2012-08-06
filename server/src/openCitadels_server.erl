-module(openCitadels_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, setup/1, register/0, quit/0,
	 players/0, status/0, status/1, move/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

setup(ClientID) ->
    gen_server:call(?SERVER, {setup, ClientID}).

register() ->
    ok.

quit() ->
    ok.

players() ->
    ok.

status() ->
    ok.

status(Player) ->
    ok.

move(Type, Args) ->
    ok.

%% ----
%% State definition
%% ----
-record(state, {
               }).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(no_args) ->
    {ok, #state{}}.


handle_call({setup, ClientID}, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


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

