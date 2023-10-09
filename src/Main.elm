-- Press a button to draw a random card.
--
-- Dependencies:
--   elm install elm/random
--


module Main exposing (..)

import Browser
import Deck exposing (Card(..), Suite(..), Value(..), cardGenerator, colorCard, newDeck, stringCard, suiteRank, viewCard)
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (..)
import PriorityQueue
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { game : Game, selected : Maybe GameStack }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = makeNewGame newDeck, selected = Nothing }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select GameStack



-- Draw ->
--   ( model
--   , Random.generate NewCard cardGenerator
--   )
-- NewCard newCard ->
--   ( model
--   , Cmd.none
--   )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { game, selected } =
            model
    in
    case msg of
        Select (TableauStack s) ->
            case selected of
                Nothing ->
                    ( { model | selected = Just (TableauStack s) }
                    , Cmd.none
                    )

                Just Talon ->
                    ( { model
                        | selected = Nothing
                        , game = doAction game (TalonPull s)
                      }
                    , Cmd.none
                    )

                Just Stock ->
                    ( { model | selected = Just (TableauStack s) }, Cmd.none )

                Just (TableauStack s0) ->
                    ( { model | selected = Nothing, game = doAction game (TableauAction s0 s) }, Cmd.none )

                Just (FoundationStack suite) ->
                    ( { model | selected = Nothing, game = doAction game (PushTo suite s) }, Cmd.none )

        Select Stock ->
            ( { model
                | selected = Nothing
                , game = doAction game TalonPush
              }
            , Cmd.none
            )

        Select Talon ->
            ( { model
                | selected = Just Talon
              }
            , Cmd.none
            )

        Select (FoundationStack suite) ->
            case selected of
                Just (TableauStack s) ->
                    ( { model | selected = Nothing, game = doAction game (PullFrom s) }, Cmd.none )

                Just Talon ->
                    ( { model | selected = Nothing, game = doAction game PullFromTalon }, Cmd.none )

                _ ->
                    ( { model | selected = Nothing }, Cmd.none )



-- tf(update (Select (TableauStack Game.Four)) (tf (update (Select (TableauStack Game.Five)) ({ game = makeNewGame newDeck, selected = Nothing })))) == { game = makeNewGame newDeck, selected = Nothing }
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


cardDiv : Bool -> Int -> Int -> PlayCard -> Html Msg
cardDiv glow top zdex pc =
    let
        c =
            case pc of
                UpFacing card ->
                    card

                DownFacing card ->
                    card

        upfacing =
            case pc of
                UpFacing _ ->
                    True

                DownFacing _ ->
                    False
    in
    div
        ([ style "z-index" (String.fromInt zdex)
         , style "position" "absolute"
         , style "background-color" "white"
         , style "font-size" "7.5rem"
         , class (stringCard c)
         , style "color" (colorCard c)
         , style "height" "8rem"
         , style "width" "5rem"
         , style "overflow" "hidden"
         , style "display" "flex"
         , style "align-items" "end"
         , style "justify-content" "center"
         , style "top" (String.fromInt top ++ "rem")
         ]
            ++ (if glow && upfacing then
                    [ style "box-shadow" "-10px -20px 12px 0px #ffe500, 10px -20px 12px 0px #ffe500"
                    ]

                else
                    []
               )
        )
        [ if upfacing then
            text (viewCard c)

          else
            img
                [ src "card1.jpg"
                , style "height" "8rem"
                , style "width" "5rem"
                ]
                []
        ]


type Position
    = TalonPostion
    | StockPostion
    | FoundationPosition Suite


stackCards : Maybe (List PlayCard) -> List PlayCard -> Msg -> Position -> Html Msg
stackCards spcs cs msg p =
    div
        [ onClick msg
        , style "width" "5rem"
        , style "height" "8rem"
        , style "background-color" "beige"
        , style "right"
            (case p of
                TalonPostion ->
                    "5rem"

                StockPostion ->
                    "0rem"

                _ ->
                    ""
            )
        , style "left"
            (case p of
                FoundationPosition suite ->
                    String.fromInt (suiteRank suite * 5) ++ "rem"

                _ ->
                    ""
            )
        ]
        (cs
            |> List.reverse
            |> List.indexedMap
                (\i c ->
                    cardDiv (spcs |> Maybe.map (\l -> l == cs) |> Maybe.withDefault False) 0 i c
                )
        )


stackCardsSpread : Maybe (List PlayCard) -> List PlayCard -> Msg -> Html Msg
stackCardsSpread spcs cs msg =
    div
        [ style "width" "14%"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "height" "8rem"
        , onClick msg
        ]
        (cs
            |> List.reverse
            |> List.indexedMap
                (\i c -> cardDiv (spcs |> Maybe.map (\l -> l == cs) |> Maybe.withDefault False) (i * 2) i c)
        )



-- VIEW


view : Model -> Html Msg
view { game, selected } =
    let
        { tableauStacks, foundationStacks, stock, talon } =
            game

        selectedStack =
            Maybe.map (\s -> getStack game s) selected

        pile =
            stackCards selectedStack

        spread =
            stackCardsSpread selectedStack
    in
    case ( foundationStacks, tableauStacks ) of
        ( { clubs, hearts, diamonds, spades }, { col1, col2, col3, col4, col5, col6, col7 } ) ->
            div []
                [ div
                    [ style
                        "display"
                        "flex"
                    , style
                        "position"
                        "relative"
                    , style "justify-content" "space-between"
                    ]
                    [ span [ style "display" "flex" ]
                        [ pile spades
                            (Select (FoundationStack Spades))
                            (FoundationPosition Spades)
                        , pile hearts
                            (Select (FoundationStack Spades))
                            (FoundationPosition Hearts)
                        , pile diamonds (Select (FoundationStack Spades)) (FoundationPosition Diamonds)
                        , pile clubs (Select (FoundationStack Clubs)) (FoundationPosition Clubs)
                        ]
                    , span [ style "display" "flex", style "width" "13rem", style "justify-content" "space-between" ]
                        [ pile talon (Select Talon) TalonPostion
                        , pile stock (Select Stock) StockPostion
                        ]
                    ]
                , div
                    [ style
                        "display"
                        "flex"
                    , style
                        "position"
                        "relative"
                    , style "justify-content" "space-between"
                    , style "margin-top" "2rem"
                    , style "height" "calc( 100vh - 10rem )"
                    ]
                    [ spread col1 (Select (TableauStack Game.One))
                    , spread col2 (Select (TableauStack Game.Two))
                    , spread col3 (Select (TableauStack Game.Three))
                    , spread col4 (Select (TableauStack Game.Four))
                    , spread col5 (Select (TableauStack Game.Five))
                    , spread col6 (Select (TableauStack Game.Six))
                    , spread col7 (Select (TableauStack Game.Seven))
                    ]
                ]



-- view : Model -> Html Msg
-- view model =
--   div []
--     [ button [ onClick Draw ] [ text "Draw" ]
--     , div [ style "font-size" "12em" ] [ text (viewCard model.card) ]
--     ]
