-module(openCitadels_game).
-behaviour(gen_fsm).

-compile(export_all). %% Fix this later!

-export([ start_link/1
        , state/1
        , pick_character/3
        , take/3
        , choose/3
        , build/3
        , end_turn/3
        ]).

-export([ init/1
        , handle_event/3
        , handle_info/3
        , handle_sync_event/4
        , terminate/3
        , code_change/4
        ]).

-record(ps,
        {player_id %The identifier of the current player
         ,current_character = none %The character this player is
         ,districts = [] %The districts the player has built
         ,hand = [] %The cards in the players hand
         ,money = 2 %The amount of money a player has
         ,effects = [] %assassinated, stolen from, other?
        }).

-record(gs,
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

state(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, state).

init(_Data) ->
    random:seed(erlang:now()),
    {ok, deal_cards, init_state(_Data)}.

%% stubs
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_event(state, _From, StateName, StateData) ->
    {reply, {StateName, StateData}, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.


terminate(_Reason, _StateName, _StateData) ->
    whatever.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% ------------------------------------------------------------------
%% Transition Definitions
%% ------------------------------------------------------------------

% Selecting Characters
pick_character(Pid, Player, Card) ->
    gen_fsm:sync_send_event(Pid, {take_card, Player, Card}).

% Take an action
take(Pid, Player, Choice) ->
    gen_fsm:sync_send_event(Pid, {take_an_action, Player, Choice}).

choose(Pid, Player, Card) ->
    gen_fsm:sync_send_event(Pid, {choose_card, Player, Card}).

build(Pid, Player, District) ->
    gen_fsm:sync_send_event(Pid, {build_district, Player, District}).

end_turn(Pid, Player, _Null) ->
    gen_fsm:sync_send_event(Pid, {end_turn, Player, _Null}).

%% ------------------------------------------------------------------
%% State Definitions
%% ------------------------------------------------------------------

deal_cards( {take_card, Player, Card}
          , _From
          , #gs{current_player = CurrentPlayer
               ,players = Players
               ,player_order = PlayerOrder
               ,character_deck = CDeck
               ,first_player = FirstPlayer} = State) ->

    CurrentPlayerID = get_player_id(CurrentPlayer, PlayerOrder),
    case {CurrentPlayerID =:= Player,
          lists:member(Card, CDeck)} of
        {true, true} -> %Correct player's turn, card is available
            PlayerState = get_ps(CurrentPlayerID, State),
            NewPlayerState = 
                PlayerState#ps{current_character = Card},
            OtherPlayers = lists:delete(PlayerState, Players),
            NewDeck = lists:delete(Card, CDeck),
            NextPlayer = (CurrentPlayer rem length(PlayerOrder)) + 1,
            NewState = State#gs{current_player = NextPlayer
                                       ,players = [NewPlayerState | OtherPlayers]
                                       ,character_deck = NewDeck},

            case NextPlayer =/= FirstPlayer of
               true  -> {reply, ok, deal_cards, NewState};
               false -> {reply, ok, take_an_action, pre_play_init(NewState)}
            end;
        {true, false} ->
            {reply, {error, no_card}, deal_cards, State};
        {false, _} ->
            {reply, {error, wrong_player}, deal_cards, State}
    end.

take_an_action({take_an_action, PlayerID, gold}, _From, 
	       #gs{character_order = [{_Char, PlayerID} | _]} = State) ->
    PlayerState = get_ps(PlayerID, State),
    Money = PlayerState#ps.money,
    NewState = update_ps(PlayerState#ps{money = Money + 2}, State),
    {reply, ok, build_district, NewState};
take_an_action({take_an_action, PlayerID, cards}, _From, 
	       #gs{character_order = [{_Char, PlayerID} | _]} = State) ->
    {reply, ok, take_an_action_2, State};
take_an_action(_, _, State) ->
    {reply, {error, 'WRONG!'}, take_an_action, State}.
    

take_an_action_2({choose_card, PlayerID, Card}, _From,
		 #gs{character_order = [{_Char, PlayerID} | _],
		    district_deck = [C1, C2 | Deck]} = State) ->
    case lists:member(Card, [C1, C2]) of
	true -> PS = get_ps(PlayerID, State),
		NewPS = PS#ps{hand = [Card | PS#ps.hand]},
		NewState = update_ps(NewPS, State#gs{district_deck = Deck}),
		{reply, ok, build_district, NewState};
	false -> {reply, {error, bad_card}, take_an_action_2, State}
    end;
take_an_action_2(_, _, State) ->
    {reply, {error, 'WRONG!'}, take_an_action_2, State}.

build_district({build_district, PlayerID, Card}, _From,
	       #gs{character_order = [{_Char, PlayerID} | _]} = State) ->
    PS = get_ps(PlayerID, State),
    Hand = PS#ps.hand,
    Tab = PS#ps.districts,
    Money = PS#ps.money,
    Districts = PS#ps.districts,
    Cost = district_cost(Card),
    case { lists:member(Card, Hand) andalso not lists:member(Card, Tab)
	     , Cost =< Money} of
	{false, _} ->
	    {reply, {error, bad_card}, build_district, State};
	{_, false} ->
	    {reply, {error, no_money}, build_district, State};
	{true, true} ->
	    NewPS = PS#ps{hand = lists:delete(Card, Hand),
			  money = Money - Cost,
			  districts = [Card | Districts]},
	    NewState = update_ps(NewPS, State),
	    {reply, ok, post_build, NewState}
    end;
build_district( {end_turn, PlayerID, _}
              , _From
              , #gs{character_order = [{_Char, PlayerID}]} = State) ->
    %Check for end game conditions etc.
    {reply, ok, deal_cards, pre_deal_cards(State)};
build_district( {end_turn, PlayerID, _}
              , _From
              , #gs{character_order = [{_Char, PlayerID} | Players]} = State) ->
    {reply, ok, take_an_action, State#gs{character_order = Players}};
build_district(_, _, State) ->
    {reply, {error, 'WRONG!'}, build_district, State}.


post_build( {end_turn, PlayerID, _}
          , _From
          , #gs{character_order = [{_Char, PlayerID}]} = State) ->
    %Check for end game conditions etc.
    {reply, ok, deal_cards, pre_deal_cards(State)};
post_build( {end_turn, PlayerID, _}
          , _From
          , #gs{character_order = [{_Char, PlayerID} | Players]} = State) ->
    {reply, ok, take_an_action, State#gs{character_order = Players}};
post_build(_, _, State) ->
    {reply, {error, 'WRONG!'}, post_build, State}.



%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

init_state(Options) ->
    DistrictDeck = shuffle_deck(district_list()),
    init_state(Options, #gs{district_deck = DistrictDeck}).
init_state([], State) ->
    pre_deal_cards(State);
init_state([{players, Players} | Options], 
           #gs{district_deck = DistrictDeck} = State) ->
    {PlayerStates, DDeck} = initialise_pss(Players, DistrictDeck),
    NewState = State#gs{players = PlayerStates
                                ,district_deck = DDeck
                                ,player_order = Players
                                ,first_player = 1
                                ,current_player = 1
                               },
    init_state(Options, NewState);
init_state([{server_pid, ServerPid} | Options], State) ->
    NewState = State#gs{server_pid = ServerPid},
    init_state(Options, NewState);
init_state([{game_id, GameID} | Options], State) ->
    NewState = State#gs{game_id = GameID},
    init_state(Options, NewState);
init_state([_ | Options], State) ->
    init_state(Options, State).

% Makes initial states for each player
initialise_pss([], DDeck) ->
    {[], DDeck};
initialise_pss([Player | Players], [D1, D2, D3, D4 | DDeck]) ->
    {PlayerList, DistrictDeck} = initialise_pss(Players, DDeck),
    {[#ps{player_id = Player, hand = [D1,D2,D3,D4]} |
      PlayerList], DistrictDeck}.

pre_play_init(#gs{players = Players} = State) ->
    %Could possibly just sort the characters, depending on representation
    Fun = fun(#ps{current_character = C, player_id = P}) -> {C, P} end,
    Selected = lists:map(Fun, Players),
    %If cards can be sorted in correct order, this would not be necessary
    SelectedInOrder = 
	lists:sort(fun ({C1, _}, {C2, _}) -> character_order(C1, C2) end, Selected),
    NewState = State#gs{character_order = SelectedInOrder},
    NewState.

% Initialise gamestate for dealing character cards
%%! incorrect for |players| < 3
pre_deal_cards(State) ->
    N = length(State#gs.players),
    [FD | Cs] = shuffle_deck(character_list()),
    FU = lists:sublist([C || C <- Cs, C =/= {4, king}], 6 - N),
    State#gs{ character_deck = Cs -- FU
            , face_down = FD
            , face_up = FU
            , character_order = []
            , current_player = State#gs.first_player
            }.

% Permutes a list (shuffles a deck)
shuffle_deck(List) ->
    ModList = lists:map(fun(E) -> {random:uniform(), E} end, List),
    SortedModList = lists:sort(ModList),
    lists:map(fun({_, E}) -> E end, SortedModList).

% Number of players in game
num_players(#gs{player_order = PlayerOrder}) ->
    length(PlayerOrder).

% Retrieves player id of player nr Player from GameState
get_player_id(Player, #gs{player_order = PlayerOrder}) ->
    lists:nth(Player, PlayerOrder);
get_player_id(Player, PlayerOrder) ->
    lists:nth(Player, PlayerOrder).

% Retrieves a player's state given the players id
get_ps(PlayerID, #gs{players = Players}) ->
    lists:keyfind(PlayerID, #ps.player_id, Players).

% Updates gamestate with the new player state
update_ps(#ps{player_id = PlayerID} = PlayerState, 
		    #gs{players = Players} = GameState) ->
    NewPlayers =
	lists:keyreplace(PlayerID, #ps.player_id, Players, PlayerState),
    GameState#gs{players = NewPlayers}.

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

% Get district cost
district_cost({C, _}) ->
    C.

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
    [First, Second, Third, Fourth, Fifth | _Rest] = 
        InitState#gs.character_deck,
    {_, _, NewState1} =
        deal_cards({take_card, 
                    get_player_id(InitState#gs.current_player, InitState), 
                    First}, lol,
                   InitState),
    {_, _, NewState2} =
        deal_cards({take_card,
                    get_player_id(NewState1#gs.current_player, NewState1), 
                    Second}, lol,
                   NewState1),
    {_, _, NewState3} =
        deal_cards({take_card, 
                    get_player_id(NewState2#gs.current_player, NewState2), 
                    Third},lol ,
                   NewState2),
    {_, _, NewState4} =
        deal_cards({take_card, 
                    get_player_id(NewState3#gs.current_player, NewState3), 
                    Fourth},lol,
                   NewState3),
    %{_, _, NewState} =
        deal_cards({take_card, 
                    get_player_id(NewState4#gs.current_player, NewState4), 
                    Fifth},lol,
                   NewState4).

    %NewState.
