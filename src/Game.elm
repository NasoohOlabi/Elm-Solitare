module Game exposing (..)

import Deck exposing (..)
import List exposing (..)
import Util exposing (..)


type PlayCard
    = UpFacing Card
    | DownFacing Card


type TableauStackNumber
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven


type GameStack
    = TableauStack TableauStackNumber
    | FoundationStack Suite
    | Talon
    | Stock


getStack : Game -> GameStack -> List PlayCard
getStack g s =
    case s of
        TableauStack number ->
            case g.tableauStacks of
                { col1, col2, col3, col4, col5, col6, col7 } ->
                    case number of
                        One ->
                            col1

                        Two ->
                            col2

                        Three ->
                            col3

                        Four ->
                            col4

                        Five ->
                            col5

                        Six ->
                            col6

                        Seven ->
                            col7

        FoundationStack suite ->
            case g.foundationStacks of
                { clubs, hearts, diamonds, spades } ->
                    case suite of
                        Clubs ->
                            clubs

                        Hearts ->
                            hearts

                        Diamonds ->
                            diamonds

                        Spades ->
                            spades

        Talon ->
            g.talon

        Stock ->
            g.stock


setStack : GameStack -> List PlayCard -> Game -> Game
setStack s lst g =
    case s of
        FoundationStack suite ->
            let
                { foundationStacks } =
                    g
            in
            case suite of
                Clubs ->
                    { g | foundationStacks = { foundationStacks | clubs = lst } }

                Hearts ->
                    { g | foundationStacks = { foundationStacks | hearts = lst } }

                Diamonds ->
                    { g | foundationStacks = { foundationStacks | diamonds = lst } }

                Spades ->
                    { g | foundationStacks = { foundationStacks | spades = lst } }

        TableauStack number ->
            let
                { tableauStacks } =
                    g
            in
            case number of
                One ->
                    { g | tableauStacks = { tableauStacks | col1 = lst } }

                Two ->
                    { g | tableauStacks = { tableauStacks | col2 = lst } }

                Three ->
                    { g | tableauStacks = { tableauStacks | col3 = lst } }

                Four ->
                    { g | tableauStacks = { tableauStacks | col4 = lst } }

                Five ->
                    { g | tableauStacks = { tableauStacks | col5 = lst } }

                Six ->
                    { g | tableauStacks = { tableauStacks | col6 = lst } }

                Seven ->
                    { g | tableauStacks = { tableauStacks | col7 = lst } }

        Talon ->
            { g | talon = lst }

        Stock ->
            { g | stock = lst }


type alias TableauStacks =
    { col1 : List PlayCard
    , col2 : List PlayCard
    , col3 : List PlayCard
    , col4 : List PlayCard
    , col5 : List PlayCard
    , col6 : List PlayCard
    , col7 : List PlayCard
    }



-- getTableauStack


type alias FoundationStacks =
    { clubs : List PlayCard
    , hearts : List PlayCard
    , diamonds : List PlayCard
    , spades : List PlayCard
    }


type alias Game =
    { foundationStacks :
        FoundationStacks
    , tableauStacks :
        TableauStacks
    , stock : List PlayCard
    , talon : List PlayCard
    }


mkTableauStacks : Deck -> List (List PlayCard) -> TableauStacks
mkTableauStacks d acc =
    let
        len =
            List.length acc + 1
    in
    if len <= 7 then
        mkTableauStacks (List.drop len d)
            (acc
                ++ [ d
                        |> List.take len
                        |> List.indexedMap
                            (\i c ->
                                if 0 == i then
                                    UpFacing c

                                else
                                    DownFacing c
                            )
                   ]
            )

    else
        case acc of
            p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: _ ->
                { col1 = p1
                , col2 = p2
                , col3 = p3
                , col4 = p4
                , col5 = p5
                , col6 = p6
                , col7 = p7
                }

            _ ->
                { col1 = []
                , col2 = []
                , col3 = []
                , col4 = []
                , col5 = []
                , col6 = []
                , col7 = []
                }


makeNewGame : Deck -> Game
makeNewGame d =
    { foundationStacks =
        { clubs = [], hearts = [], diamonds = [], spades = [] }
    , tableauStacks = mkTableauStacks (List.take 28 d) []
    , stock = d |> List.drop 28 |> List.map (\c -> DownFacing c)
    , talon = []
    }


type GameAction
    = TableauAction TableauStackNumber TableauStackNumber
    | TalonPull TableauStackNumber
    | TalonEmpty
    | TalonPush
    | PullFrom TableauStackNumber
    | PullFromTalon
    | PushTo Suite TableauStackNumber


opositeSuiteColor : Suite -> Suite -> Bool
opositeSuiteColor a b =
    (listContain [ Spades, Clubs ] a
        && listContain [ Hearts, Diamonds ] b
    )
        || (listContain [ Hearts, Diamonds ] a
                && listContain [ Spades, Clubs ] b
           )


tableauDoesSitOn : Card -> Card -> Bool
tableauDoesSitOn bottom top =
    case ( bottom, top ) of
        ( Card bottomValue bottomSuite, Card topValue topSuite ) ->
            opositeSuiteColor bottomSuite topSuite
                && (valueRank bottomValue
                        - 1
                        == valueRank topValue
                   )


foundationDoesSitOn : Card -> Card -> Bool
foundationDoesSitOn bottom top =
    case ( bottom, top ) of
        ( Card bottomValue bottomSuite, Card topValue topSuite ) ->
            bottomSuite
                == topSuite
                && (valueRank bottomValue
                        + 1
                        == valueRank topValue
                   )


flipCard : PlayCard -> PlayCard
flipCard c =
    case c of
        UpFacing card ->
            DownFacing card

        DownFacing card ->
            UpFacing card


flipFirst : List PlayCard -> List PlayCard
flipFirst l =
    case l of
        h :: rest ->
            flipCard h :: rest

        _ ->
            l


flipIfNeeded : List PlayCard -> List PlayCard
flipIfNeeded l =
    case l of
        (DownFacing card) :: rest ->
            UpFacing card :: rest

        _ ->
            l



--  maybeDoAction (makeNewGame newDeck) (TableauAction (Game.Four) ( Game.Five))


maybeDoAction : Game -> GameAction -> Maybe Game
maybeDoAction g a =
    case a of
        TableauAction s1 s2 ->
            let
                source =
                    getStack g (TableauStack s1)

                target =
                    getStack g (TableauStack s2)
            in
            case ( source, target ) of
                ( (UpFacing (Card King suite)) :: new_s1, [] ) ->
                    Just
                        (g
                            |> setStack (TableauStack s1) (flipFirst new_s1)
                            |> setStack (TableauStack s2) [ UpFacing (Card King suite) ]
                        )

                ( _ :: _, (UpFacing bottom) :: _ ) ->
                    case
                        find source
                            (\e ->
                                case e of
                                    UpFacing card ->
                                        tableauDoesSitOn bottom card

                                    _ ->
                                        False
                            )
                    of
                        Just ind ->
                            Just
                                (g
                                    |> setStack (TableauStack s1) (flipIfNeeded <| List.drop (ind + 1) source)
                                    |> setStack (TableauStack s2)
                                        (List.take (ind + 1) source
                                            ++ target
                                        )
                                )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        TalonPull s ->
            case ( g.talon, getStack g (TableauStack s) ) of
                ( (UpFacing top) :: talon_rest, (UpFacing bottom) :: rest ) ->
                    if tableauDoesSitOn bottom top then
                        Just
                            (g
                                |> setStack (TableauStack s)
                                    (UpFacing top
                                        :: UpFacing bottom
                                        :: rest
                                    )
                                |> setStack Talon talon_rest
                            )

                    else
                        Nothing

                ( (UpFacing (Card King suite)) :: talon_rest, [] ) ->
                    Just
                        (g
                            |> setStack (TableauStack s)
                                [ UpFacing (Card King suite) ]
                            |> setStack Talon talon_rest
                        )

                _ ->
                    Nothing

        TalonEmpty ->
            if List.length g.stock == 0 then
                Just { g | talon = [], stock = List.map flipCard <| List.reverse <| g.talon }

            else
                Nothing

        TalonPush ->
            case g.stock of
                (DownFacing card) :: rest ->
                    Just { g | talon = UpFacing card :: g.talon, stock = rest }

                _ ->
                    maybeDoAction g TalonEmpty

        PushTo suite stack ->
            case ( getStack g (FoundationStack suite), getStack g (TableauStack stack) ) of
                ( (UpFacing top) :: new_foundation, (UpFacing bottom) :: rest ) ->
                    if tableauDoesSitOn bottom top then
                        Just
                            (g
                                |> setStack (FoundationStack suite) new_foundation
                                |> setStack (TableauStack stack)
                                    (UpFacing top
                                        :: UpFacing bottom
                                        :: rest
                                    )
                            )

                    else
                        Nothing

                ( (UpFacing (Card King _)) :: new_foundation, [] ) ->
                    Just
                        (g
                            |> setStack (FoundationStack suite) new_foundation
                            |> setStack (TableauStack stack) [ UpFacing (Card King suite) ]
                        )

                _ ->
                    Nothing

        PullFromTalon ->
            case g.talon of
                (UpFacing (Card Ace suite)) :: rest ->
                    Just
                        (g
                            |> setStack (FoundationStack suite) [ UpFacing (Card Ace suite) ]
                            |> setStack Talon rest
                        )

                (UpFacing (Card v suite)) :: rest ->
                    case getStack g (FoundationStack suite) of
                        (UpFacing bottom) :: foundation ->
                            let
                                top =
                                    Card v suite
                            in
                            if foundationDoesSitOn bottom top then
                                Just
                                    (g
                                        |> setStack (FoundationStack suite) (UpFacing top :: UpFacing bottom :: foundation)
                                        |> setStack Talon rest
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        PullFrom stack ->
            case getStack g (TableauStack stack) of
                (UpFacing (Card Ace suite)) :: rest ->
                    Just
                        (g
                            |> setStack (FoundationStack suite) [ UpFacing (Card Ace suite) ]
                            |> setStack (TableauStack stack) (flipFirst rest)
                        )

                (UpFacing (Card v suite)) :: rest ->
                    case getStack g (FoundationStack suite) of
                        (UpFacing bottom) :: _ ->
                            if foundationDoesSitOn bottom (Card v suite) then
                                Just
                                    (g
                                        |> setStack (TableauStack stack) (flipIfNeeded rest)
                                        |> setStack (FoundationStack suite)
                                            (UpFacing (Card v suite)
                                                :: getStack g (FoundationStack suite)
                                            )
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing


doAction : Game -> GameAction -> Game
doAction g a =
    Maybe.withDefault g (maybeDoAction g a)



-- neighbors : Game -> List Game
-- neighbors g =
