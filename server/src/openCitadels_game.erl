-module(openCitadels_game).
-behaviour(gen_fsm).

-compile(export_all). %% Fix this later!

-record(player_state,
        {player_id %The identifier of the current player
         ,current_character = none %The character this player is
         ,districts = [] %The districts the player has built
         ,hand = [] %The cards in the players hand
         ,money = 2 %The amount of money a player has
         ,effects = [] %assassinated, stolen from, other?
        }).

-record(game_state,
        {game_id %How a game instance is identified by the server (static)
         ,server_pid %For communicating errors and the like (static)
         ,player_order = [] %The order the players go (static)
         ,current_player = 1 %The current player, relative to player_order
         ,character_order = [] %During play, shows the selected characters' order
         ,players = [] %List of player states
         ,first_player = 1 %First player (is changed after a round)
         ,district_deck = [] %Deck of districts
         ,character_deck = [] %Deck of characters to select from
         ,face_down = [] %The face down characters
         ,face_up = [] %The face up characters
        }).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

start_link(Data) ->
    gen_fsm:start_link(?MODULE, Data, []).

init(_Data) ->
    random:seed(erlang:now()),
    {ok, select_character}.

%% ------------------------------------------------------------------
%% Transition Definitions
%% ------------------------------------------------------------------

%Selecting Characters
take_card(Pid, Player, Card) ->
    gen_fsm:send_event(Pid, {take_card, Player, Card}).

%% ------------------------------------------------------------------
%% State Definitions
%% ------------------------------------------------------------------

deal_cards({take_card, Player, Card}, 
           #game_state{current_player = CurrentPlayer
                       ,players = Players
                       ,player_order = PlayerOrder
                       ,character_deck = CDeck
                       ,first_player = FirstPlayer} = State) ->

    CurrentPlayerID = get_player_id(CurrentPlayer, PlayerOrder),
    case {CurrentPlayerID =:= Player,
          lists:member(Card, CDeck)} of
        {true, true} -> %Correct player's turn, card is available
            PlayerState = get_player_state(CurrentPlayerID, State),
            NewPlayerState = 
                PlayerState#player_state{current_character = Card},
            OtherPlayers = lists:delete(PlayerState, Players),
            NewDeck = lists:delete(Card, CDeck),
            NextPlayer = (CurrentPlayer rem length(PlayerOrder)) + 1,
            NewState = State#game_state{current_player = NextPlayer
                                       ,players = [NewPlayerState | OtherPlayers]
                                       ,character_deck = NewDeck},

            if NextPlayer =/= FirstPlayer -> 
                    {next_state, deal_cards, NewState};
               true -> play(NewState)
            end;

        _ -> {next_state, deal_cards, State}
    end.

pre_play_init(#game_state{players = Players} = State) ->
    %Could possibly just sort the characters, depending on representation
    Fun = fun(#player_state{current_character = C, player_id = P}) -> {C, P} end,
    Selected = lists:map(Fun, Players),
    %If cards can be sorted in correct order, this would not be necessary
    SelectedInOrder = 
	lists:sort(fun ({C1, _}, {C2, _}) -> character_order(C1, C2) end, Selected),
    NewState = State#game_state{character_order = SelectedInOrder},
    NewState.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_state(Options) ->
    DistrictDeck = shuffle_deck(district_list()),
    init_state(Options, #game_state{district_deck = DistrictDeck}).
init_state([], State) ->
    pre_deal_cards(State);
init_state([{players, Players} | Options], 
           #game_state{district_deck = DistrictDeck} = State) ->
    {PlayerStates, DDeck} = initialise_player_states(Players, DistrictDeck),
    NewState = State#game_state{players = PlayerStates
                                ,district_deck = DDeck
                                ,player_order = Players
                                ,first_player = 1
                                ,current_player = 1
                               },
    init_state(Options, NewState);
init_state([{server_pid, ServerPid} | Options], State) ->
    NewState = State#game_state{server_pid = ServerPid},
    init_state(Options, NewState);
init_state([{game_id, GameID} | Options], State) ->
    NewState = State#game_state{game_id = GameID},
    init_state(Options, NewState);
init_state([_ | Options], State) ->
    init_state(Options, State).

% Makes initial states for each player
initialise_player_states([], DDeck) ->
    {[], DDeck};
initialise_player_states([Player | Players], [D1, D2, D3, D4 | DDeck]) ->
    {PlayerList, DistrictDeck} = initialise_player_states(Players, DDeck),
    {[#player_state{player_id = Player, hand = [D1,D2,D3,D4]} |
      PlayerList], DistrictDeck}.

% Retrieves player id of player nr Player from GameState
get_player_id(Player, #game_state{player_order = PlayerOrder}) ->
    lists:nth(Player, PlayerOrder);
get_player_id(Player, PlayerOrder) ->
    lists:nth(Player, PlayerOrder).

% Retrieves a player's state given the players id
get_player_state(PlayerID, #game_state{players = Players}) ->
    lists:keyfind(PlayerID, #player_state.player_id, Players).
%    {[PlayerState], _} = 
%       lists:partition(fun(P) -> P#player_state.player_id =:= PlayerID end, Players).

% Initialise gamestate for dealing character cards
pre_deal_cards(GameState) ->
    pre_deal_cards(GameState, num_players(GameState)).
pre_deal_cards(GameState, 5) ->
    [FaceDown , FaceUp , MaybeFaceUp | CDeck] = shuffle_deck(character_list()),
    case FaceUp of
        {4, king} ->
            GameState#game_state{character_deck = [FaceUp | CDeck]
                                 ,face_down = [FaceDown]
                                 ,face_up = [MaybeFaceUp]};
        _ ->
            GameState#game_state{character_deck = [MaybeFaceUp | CDeck]
                                 ,face_down = [FaceDown]
                                 ,face_up = [FaceUp]}
    end;
pre_deal_cards(_, _) ->
    {error, "No rules for this amount of players."}.

% Permutes a list (shuffles a deck)
shuffle_deck(List) ->
    ModList = lists:map(fun(E) -> {random:uniform(), E} end, List),
    SortedModList = lists:sort(ModList),
    lists:map(fun({_, E}) -> E end, SortedModList).

num_players(#game_state{player_order = PlayerOrder}) ->
    length(PlayerOrder).

%% ------------------------------------------------------------------
%% Test Functions
%% ------------------------------------------------------------------

% Sort characters function
character_order(C1, C2) ->
    C1 =< C2.

% Get list of characters in the game
character_list() ->
    [{1, assassin}
     ,{2, thief}
     ,{3, magician}
     ,{4, king}
     ,{5, bishop}
     ,{6, merchant}
     ,{7, architect}
     ,{8, warlord}].

% Get list of district cards in the game
district_list() ->
    Base = [{X, Y} || X <- [1,2,3,4,5],
                      Y <- [green, yellow, red, blue]],
    Purple = [{X, purple} || X <- [3,4,5,6]],
    Base ++ Base ++ Purple ++ Purple.

% Generic option list
option_list() ->
    [{players, [a,b,c,d,e]}
     ,{server_pid, self()}
    ].

test() ->
    InitState = init_state(option_list()),
    [First, Second, Third, Fourth, Fifth | Rest] = 
        InitState#game_state.character_deck,
    {_, _, NewState1} =
        deal_cards({take_card, 
                    get_player_id(InitState#game_state.current_player, InitState), 
                    First},
                   InitState),
    {_, _, NewState2} =
        deal_cards({take_card,
                    get_player_id(NewState1#game_state.current_player, NewState1), 
                    Second},
                   NewState1),
    {_, _, NewState3} =
        deal_cards({take_card, 
                    get_player_id(NewState2#game_state.current_player, NewState2), 
                    Third},
                   NewState2),
    {_, _, NewState4} =
        deal_cards({take_card, 
                    get_player_id(NewState3#game_state.current_player, NewState3), 
                    Fourth},
                   NewState3),
    %{_, _, NewState} =
        deal_cards({take_card, 
                    get_player_id(NewState4#game_state.current_player, NewState4), 
                    Fifth},
                   NewState4).

    %NewState.
