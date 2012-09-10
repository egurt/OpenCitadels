-module(openCitadels_game).
-behaviour(gen_server).

-compile(export_all). %% Fix this later!

-define(END_DSTR, 8).

-define(ASSASSIN, {1, assassin}).
-define(THIEF, {2, thief}).
-define(MAGICIAN, {3, magician}).
-define(KING, {4, king}).
-define(BISHOP, {5, bishop}).
-define(MERCHANT, {6, merchant}).
-define(ARCHITECT, {7, architect}).
-define(WARLORD, {8, warlord}).


-export([ start_link/1
        , state/1
        , do/3
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(ps,
        { player_id %The identifier of the current player
        , current_character = none %The character this player is
        , districts = [] %The districts the player has built
        , hand = [] %The cards in the players hand
        , money = 2 %The amount of money a player has
        , effects = [] %assassinated, stolen from, character abilities
        , actions = []
        , has_built = false
        }).

-record(gs,
        { game_id %How a game instance is identified by the server (static)
        , seed
        , server_pid %For communicating errors and the like (static)
        , player_order = [] %The order the players go (static)
        , current_player = 1 %The current player, relative to player_order
        , character_order = [] %During play, shows the selected characters' order
        , players = [] %List of player states
        , first_player = 1 %First player (is changed after a round)
        , district_deck = [] %Deck of districts
        , character_deck = [] %Deck of characters to select from
        , face_down = [] %The face down characters
        , face_up = [] %The face up characters
        , action_store
        }).

%special effects (both for districts and characters)
-record(spc,
        { prio = 10 % priority (matters only for instants, low value -> high prio)
        , state = build % when can this effect be invoked
        , how = anytime % is it a must-happen or selectable (anytime/instant)
        , name % name of the effect
        }).

-define(SEND, openCitadels_server:send).
%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

start_link(Data) ->
    gen_server:start_link(?MODULE, [{server_pid, self()} | Data], []).

state(Pid) ->
    gen_server:call(Pid, state).

do(Pid, PID, Action) ->
    gen_server:call(Pid, {do, PID, Action}).

status(Pid) ->
    gen_server:call(Pid, status).

init(Data) ->
    %% if no seed, default to now()
    Seed = proplists:get_value(seed, Data, erlang:now()),
    random:seed(Seed),
    {players, PlayerIDs} = proplists:lookup(players, Data),
    {game_id, GameID} = proplists:lookup(game_id, Data),
    Districts = shuffle_deck(district_list()),
    {PSs, Deck} = lists:foldr(fun init_ps/2, {[], Districts}, PlayerIDs),
    State = #gs{ players        = PSs
               , district_deck  = Deck
               , player_order   = PlayerIDs
               , first_player   = 1
               , current_player = 1
               , server_pid     = proplists:get_value(server_pid, Data)
               , game_id        = GameID
               , seed           = Seed
               },
    {ok, pre_deal_cards(State)}.

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(status, _From, #gs{players = Players} = State) ->
    L = lists:flatten (
        [ [ {player, P}
          , {money, M}
          , {districts, D}
          , {hand, H}
          , {actions, A}
          ]
          ||
            #ps{ player_id = P
               , money     = M
               , districts = D
               , hand      = H
               , actions   = A
               } <- Players
        ]),
    {reply, L, State};

% select a character
handle_call({do, PlayerID, {choose, Card} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            #gs{ current_player = CurrentPlayer
               , players        = Players
               , character_deck = CDeck
               , first_player   = FirstPlayer
               , game_id        = GameID
               } = State,
            NewCDeck = CDeck -- [Card],
            NextPlayer = (CurrentPlayer rem length(Players)) + 1,
            Choices = [{choose, Char} || Char <- NewCDeck],
            NextPS = lists:nth(NextPlayer, Players),
            NewPS = PS#ps{ actions = []
                         , current_character = Card
                         },
            NewGS =
                update_ps(NewPS, State),
            NewerGS = case NextPlayer =:= FirstPlayer of
                true ->
                    pre_turn(pre_play_init(NewGS));
                false ->
                    ?SEND(GameID, NextPS#ps.player_id, {actions, Choices}),
                    update_ps(NextPS#ps{actions = Choices}, NewGS)
            end,
            {reply, ok, NewerGS#gs{character_deck = NewCDeck, current_player = NextPlayer}};
        false ->
            {reply, {error, bad_action}, State}
    end;
% take an action (take gold)
handle_call({do, PlayerID, {take, gold} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Money = PS#ps.money + 2,
            NewPS = set_actions(build, PS#ps{money = Money}),
            NewState = update_ps(NewPS, State),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, NewState};
        false ->
            {reply, {error, bad_action}, State}
    end;
% take an action (take cards, choose 1 of 2)
handle_call({do, PlayerID, {take, cards}}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member({take, cards}, PS#ps.actions) of
        true ->
            [C1, C2 | _] = State#gs.district_deck,
            Actions = [{pick, C} || C <- [C1, C2]],
            NewState = update_ps(PS#ps{actions = Actions}, State),
            ?SEND(State#gs.game_id, PlayerID, {actions, Actions}),
            {reply, ok, NewState#gs{action_store = PS#ps.actions}};
        false ->
            {reply, {error, bad_action}, State}
    end;
% choose which card to keep during take an action (take card)
handle_call({do, PlayerID, {pick, Card} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            PS = get_ps(PlayerID, State),
            [_C1, _C2 | NewDeck] = State#gs.district_deck,
            NewHand = [Card | PS#ps.hand],
            NewPS = set_actions(build, PS#ps{ hand = NewHand
                                            , actions = State#gs.action_store}),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State#gs{district_deck = NewDeck})};
        false ->
            {reply, {error, bad_action}, State}
    end;
% build district
handle_call({do, PlayerID, {build, Card} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Filter = fun ({build, _}) -> false; (_) -> true end,
            NewPS = PS#ps{ hand      = lists:delete(Card, PS#ps.hand)
                         , money     = PS#ps.money - district_cost(Card)
                         , districts = [Card | PS#ps.districts]
                         , actions   = [A || A <- PS#ps.actions, Filter(A)]
                         , has_built = true
                         },
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State)};
        false ->
            {reply, {error, bad_action}, State}
    end;
% end turn
handle_call({do, PlayerID, end_turn = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            GID = State#gs.game_id,
            %Check for end round/game conditions etc.
            %Should only be done when the last character has done his thing?
            case game_over(State) of
                true ->
                    ?SEND(GID, all, {game_over, player_points(all, State)}),
                    {reply, ok, State};
                false ->
                    ?SEND(GID, PlayerID, {actions, []}),
                    {reply, ok, pre_turn(update_ps(PS#ps{actions = [],
                                                         has_built = false},
                                                   State))}
            end;
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: Assassin, assassinate
handle_call({do, PlayerID, {assassinate, Target} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            #gs{players = Players} = State,
            Filter = fun(#spc{name = {assassinate, _}}) -> false;
                        ({assassinate, _}) -> false;
                        (_) -> true end,
            Eff = lists:filter(Filter, PS#ps.effects),
            Act = lists:filter(Filter, PS#ps.actions),
            NewPS = PS#ps{effects = Eff, actions = Act},

            %targetted player gets effect instant end_turn (assassinated)
            case lists:keyfind(Target, #ps.current_character, Players) of
                false ->
                    NewState = update_ps(NewPS, State);
                TarPS ->
                    TarEff = TarPS#ps.effects,
                    Ass = #spc{ prio = 1
                              , state = start
                              , how = instant
                              , name = end_turn
                              },
                    NewState = update_ps(TarPS#ps{effects = [Ass | TarEff]},
                                         update_ps(NewPS, State))
            end,
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, NewState)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: Thief, steal_from
handle_call({do, PlayerID, {steal_from, Target} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            #gs{players = Players} = State,
            Filter = fun(#spc{name = {steal_from, _}}) -> false;
                        ({steal_from, _}) -> false;
                        (_) -> true end,
            Eff = lists:filter(Filter, PS#ps.effects),
            Act = lists:filter(Filter, PS#ps.actions),
            NewPS = PS#ps{effects = Eff, actions = Act},

            %targetted player gets effect stolen_from
            case lists:keyfind(Target, #ps.current_character, Players) of
                false ->
                    NewState = update_ps(NewPS, State);
                TarPS ->
                    TarEff = TarPS#ps.effects,
                    Stl = #spc{ prio = 2
                              , state = start
                              , how = instant
                              , name = stolen_from
                              },
                    NewState = update_ps(TarPS#ps{effects = [Stl | TarEff]},
                                         update_ps(NewPS, State))
            end,
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, NewState)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Effect: got stolen from thief
handle_call({do, PlayerID, stolen_from = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            #gs{players = Players} = State,
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            Gold = PS#ps.money,
            NewPS = set_actions(start, PS#ps{effects = Eff, money = 0}),

            %thief player
            Thief = lists:keyfind(?THIEF, #ps.current_character, Players),
            ThGold = Thief#ps.money,

            NewState = update_ps(Thief#ps{money = ThGold + Gold},
                                 update_ps(NewPS, State)),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, NewState)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: King, take crown
handle_call({do, PlayerID, take_crown = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            NewPS = set_actions(start, PS#ps{effects = Eff}),
            PO = State#gs.player_order,
            NewState = State#gs{first_player = index(PlayerID, PO)},
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, NewState)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: King, Bishop, Merchant, Warlord, take money of specific color
handle_call({do, PlayerID, {take_gold, Color} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            Act = lists:delete(Action, PS#ps.actions),
            Filter = fun(C) -> district_color(C) =:= Color end,
            Gold = PS#ps.money + length(lists:filter(Filter, PS#ps.districts)),
            NewAct = Act ++
                [ {build, D} || D <- PS#ps.hand
                              , not(PS#ps.has_built)
                              , district_cost(D) > PS#ps.money
                              , district_cost(D) =< Gold ],

            NewPS = PS#ps{ effects = Eff
                         , actions = NewAct
                         , money = Gold
                         },

            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: Merchant, take one gold after take an action
handle_call({do, PlayerID, take_one_gold = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            Gold = PS#ps.money + 1,
            NewPS = set_actions(build,
                                PS#ps{ effects = Eff
                                     , money = Gold
                                     }),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State)};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: Architect, take two cards after take an action
handle_call({do, PlayerID, take_two_cards = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            [ C1, C2 | Dstr ] = State#gs.district_deck,
            NewPS = set_actions(build,
                                PS#ps{ effects = Eff
                                     , hand = [ C1, C2 | PS#ps.hand ]
                                     }),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State#gs{district_deck = Dstr})};
        false ->
            {reply, {error, bad_action}, State}
    end;

% Special ability: Warlord, enter 'destroy a district' mode
handle_call({do, PlayerID, destroy_district = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            Eff = lists:keydelete(Action, #spc.name, PS#ps.effects),
            Store = lists:delete(Action, PS#ps.actions),
            Foldr = fun(#spc{name = {immune_to_warlord, Char}}, L) -> [ Char | L ];
                       (_, L) -> L
                    end,
            Immune = lists:foldr(Foldr, [], PS#ps.effects),
            Act =
                [cancel | [ {destroy, D, P}
                || PSS <- State#gs.players
                ,  not(lists:member(PSS#ps.current_character, Immune))
                ,  P = PSS#ps.player_id
                ,  D <- PSS#ps.districts
                ,  district_cost(D) =< PS#ps.money + 1
                ]],
            NewPS =
                PS#ps{ effects = Eff
                     , actions = Act
                     },
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State#gs{action_store = Store})};
        false ->
            {reply, {error, bad_action}, State}
    end;

handle_call({do, PlayerID, {destroy, D, P} = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            TargetPS = get_ps(P, State),
            TargetDS = lists:delete(D, TargetPS#ps.districts),
            Store = State#gs.action_store,
            NewMoney = PS#ps.money - district_cost(D) + 1,
            NewPS = PS#ps{ money = NewMoney
                         , actions = Store
                         },
            NewState = update_ps(TargetPS#ps{districts = TargetDS},
                                 update_ps(NewPS, State)),
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, NewState};
        false ->
            {reply, {error, bad_action}, State}
    end;

handle_call({do, PlayerID, cancel = Action}, _From, State) ->
    PS = get_ps(PlayerID, State),
    case lists:member(Action, PS#ps.actions) of
        true ->
            NewPS = PS#ps{actions = State#gs.action_store},
            ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
            {reply, ok, update_ps(NewPS, State)};
        false ->
            {reply, {error, bad_action}, State}
    end;

handle_call(Message, _From, State) ->
    io:format("Unmatched call: ~p\n", [Message]),
    {reply, ok, State}.


handle_cast(_Message, State) ->
    {noreply, State}.


handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    whatever.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% End game test
game_over(#gs{players = Players}) ->
    Dstr = lists:map(fun(PS) -> PS#ps.districts end, Players),
    DstrNum = lists:map(fun length/1, Dstr),
    lists:any(fun(N) -> N >= ?END_DSTR end, DstrNum).

% Get player points
player_points(#ps{districts = Dstr}) ->
    L = lists:map(fun district_cost/1, Dstr),
    lists:sum(L).
player_points(all, #gs{player_order = PO} = State) ->
    Fun = fun(PID) -> {PID, player_points(PID, State)} end,
    lists:map(Fun, PO);
player_points(PlayerID, GameState) ->
    player_points(get_ps(PlayerID, GameState)).


pre_turn(#gs{ character_order = [ {_Char, PlayerID} | CO ]
            , players = Players
            } = State) ->
    PS = lists:keyfind(PlayerID, #ps.player_id, Players),
    NewPS = set_actions(start, PS),
    ?SEND(State#gs.game_id, PlayerID, {actions, NewPS#ps.actions}),
    update_ps(NewPS, State#gs{ character_order = CO });
pre_turn(State) ->
    pre_deal_cards(State).

% {priority, which state it applies, instant or selectable, name}
% priority is ordered such that LOWEST NUMBER HIGHEST PRIORITY!!!!
char_abilities(?ASSASSIN) ->
    [ #spc{ state = build
          , prio = 1
          , name = {assassinate, X}} ||
              X <- tl(character_list())
    ];
char_abilities(?THIEF) ->
    [ #spc{ state = build
          , prio = 1
          , name = {steal_from, X}} ||
              X <- tl(tl(character_list()))
    ];
char_abilities(?MAGICIAN) ->
    [];
char_abilities(?KING) ->
    [ #spc{ state = start
          , how = instant
          , prio = 0
          , name = take_crown
          }
    , #spc{ state = build
          , name = {take_gold, yellow}
          }
    ];
char_abilities(?BISHOP) ->
    [ #spc{ state = start
          , how = instant
          , prio = 3
          , name = warlord_immunity
          }
    ];
char_abilities(?MERCHANT) ->
    [ #spc{ state = build
          , how = instant
          , prio = 3
          , name = take_one_gold
          }
    , #spc{ state = build
          , name = {take_gold, green}
          }
    ];
char_abilities(?ARCHITECT) ->
    [ #spc{ state = build
          , how = instant
          , prio = 3
          , name = take_two_cards
          }
      % build extra districts
    ];
char_abilities(?WARLORD) ->
    [ #spc{ state = build
          , name = destroy_district
          }
    , #spc{ state = build
          , name = {take_gold, red}
          }
    , #spc{ state = all
          , name = {immun_to_warlord, ?WARLORD}
          }
    , #spc{ state = all
          , name = {immun_to_warlord, ?BISHOP}
          }
    ];
char_abilities(_) ->
    [].

dstr_abilities(_) ->
    [].

%make player (and district) special abilities enter the effect list for all players
% call after all characters have been selected but before an actual turn starts
init_player_effects(#gs{players = Players} = GS) ->
    Upd =
        fun (#ps{current_character = Char} = PS, G) ->
                Chr = char_abilities(Char),
                Dst = dstr_abilities(void),
                NewPS = PS#ps{effects = Chr ++ Dst},
                update_ps(NewPS, G) end,
    lists:foldr(Upd, GS, Players).

set_actions(start, #ps{effects = Eff} = PS) ->
    FltStart = fun(#spc{state = start}) -> true; (_) -> false end,
    FltInst = fun(#spc{how = instant}) -> true; (_) -> false end,
    Map = fun(#spc{name = Name}) -> Name end,
    Start = lists:filter(FltStart, Eff),
    Instant = lists:sort(lists:filter(FltInst, Start)),
    case Instant of
        [I | _] ->
            PS#ps{actions = [Map(I)]};
        [] ->
            NewStart = lists:map(Map, Start),
            PS#ps{actions = [{take, gold}, {take, cards} | NewStart]}
    end;
set_actions(build, #ps{ districts         = Tab
                      , hand              = Hand
                      , money             = Money
                      , effects           = Eff
                      } = PS) ->
    FltBuild = fun(#spc{state = build}) -> true; (_) -> false end,
    FltInst = fun(#spc{how = instant}) -> true; (_) -> false end,

    Map = fun(#spc{name = Name}) -> Name end,
    Bld = lists:filter(FltBuild, Eff),
    Instant = lists:sort(lists:filter(FltInst, Bld)),
    case Instant of
        [I | _] ->
            PS#ps{actions = [Map(I)]};
        [] ->
            NewBuild = lists:map(Map, Bld),
            Build = [{build, Card} || Card <- Hand -- Tab,
                                      district_cost(Card) =< Money],
            PS#ps{actions = [end_turn | NewBuild ++ Build]}
    end.



%% foldable player-state initialiser
init_ps(PlayerID, {PSs, Deck}) ->
    {Hand, Rest} = lists:split(4, Deck),
    {[#ps{player_id = PlayerID, hand = Hand} | PSs], Rest}.

pre_play_init(#gs{players = Players} = State) ->
    %Could possibly just sort the characters, depending on representation
    Fun = fun(#ps{current_character = C, player_id = P}) -> {C, P} end,
    Selected = lists:map(Fun, Players),
    SelectedInOrder =
        lists:sort(fun ({C1, _}, {C2, _}) -> character_order(C1, C2) end, Selected),
    NewState = State#gs{character_order = SelectedInOrder},
    init_player_effects(NewState).

% Initialise gamestate for dealing character cards
%%! incorrect for |players| < 3
pre_deal_cards(#gs{players = Players} = State) ->
    N = length(Players),
    [FD | Cs] = shuffle_deck(character_list()),
    FU = lists:sublist([C || C <- Cs, C =/= ?KING], 6 - N),
    Choices = [{choose, Char} || Char <- Cs -- FU],
    FPS = lists:nth(State#gs.first_player, Players),
    ?SEND(State#gs.game_id, FPS#ps.player_id, {actions, Choices}),
    update_ps( FPS#ps{actions = Choices}
             , State#gs{ character_deck = Cs -- FU
                       , face_down = FD
                       , face_up = FU
                       , character_order = []
                       , current_player = State#gs.first_player
                       }
             ).

% Randomly permutes a list (shuffles a deck)
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

% Sort characters function
character_order(C1, C2) ->
    C1 =< C2.

% Get list of characters in the game
character_list() ->
    [ ?ASSASSIN
    , ?THIEF
    , ?MAGICIAN
    , ?KING
    , ?BISHOP
    , ?MERCHANT
    , ?ARCHITECT
    , ?WARLORD
    ].

% Get district cost
district_cost({C, _}) ->
    C.

district_color({_, C}) ->
    C.

% Get list of district cards in the game
district_list() ->
    Base = [{X, Y} || X <- [1,2,3,4,5],
                      Y <- [green, yellow, red, blue]],
    Purple = [{X, purple} || X <- [3,4,5,6]],
    Base ++ Base ++ Purple ++ Purple.

index(I, [I|_]) ->
    1;
index(I, [_, L]) ->
    1 + index(I, L);
index(_, _) ->
    false.
