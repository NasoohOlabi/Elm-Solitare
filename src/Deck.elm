-- Press a button to draw a random card.
--
-- Dependencies:
--    elm install elm/random
--


module Deck exposing (Card(..), Deck, Suite(..), Value(..), cardGenerator, cardsQuickSort, colorCard, newDeck, stringCard, suiteRank, valueRank, viewCard)

import List exposing ((::))
import Ordering exposing (Ordering)
import Random
import Random.List exposing (shuffle)
import Util exposing (quickSort)


type Suite
    = Spades
    | Hearts
    | Diamonds
    | Clubs


type Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Card
    = Card Value Suite


cardGenerator : Random.Generator Card
cardGenerator =
    Random.uniform (Card Ace Spades)
        [ Card Two Spades
        , Card Three Spades
        , Card Four Spades
        , Card Five Spades
        , Card Six Spades
        , Card Seven Spades
        , Card Eight Spades
        , Card Nine Spades
        , Card Ten Spades
        , Card Jack Spades
        , Card Queen Spades
        , Card King Spades
        , Card Ace Hearts
        , Card Two Hearts
        , Card Three Hearts
        , Card Four Hearts
        , Card Five Hearts
        , Card Six Hearts
        , Card Seven Hearts
        , Card Eight Hearts
        , Card Nine Hearts
        , Card Ten Hearts
        , Card Jack Hearts
        , Card Queen Hearts
        , Card King Hearts
        , Card Ace Diamonds
        , Card Two Diamonds
        , Card Three Diamonds
        , Card Four Diamonds
        , Card Five Diamonds
        , Card Six Diamonds
        , Card Seven Diamonds
        , Card Eight Diamonds
        , Card Nine Diamonds
        , Card Ten Diamonds
        , Card Jack Diamonds
        , Card Queen Diamonds
        , Card King Diamonds
        , Card Ace Clubs
        , Card Two Clubs
        , Card Three Clubs
        , Card Four Clubs
        , Card Five Clubs
        , Card Six Clubs
        , Card Seven Clubs
        , Card Eight Clubs
        , Card Nine Clubs
        , Card Ten Clubs
        , Card Jack Clubs
        , Card Queen Clubs
        , Card King Clubs
        ]


viewCard : Card -> String
viewCard card =
    case card of
        Card Ace Spades ->
            "🂡"

        Card Two Spades ->
            "🂢"

        Card Three Spades ->
            "🂣"

        Card Four Spades ->
            "🂤"

        Card Five Spades ->
            "🂥"

        Card Six Spades ->
            "🂦"

        Card Seven Spades ->
            "🂧"

        Card Eight Spades ->
            "🂨"

        Card Nine Spades ->
            "🂩"

        Card Ten Spades ->
            "🂪"

        Card Jack Spades ->
            "🂫"

        Card Queen Spades ->
            "🂭"

        Card King Spades ->
            "🂮"

        Card Ace Hearts ->
            "🂱"

        Card Two Hearts ->
            "🂲"

        Card Three Hearts ->
            "🂳"

        Card Four Hearts ->
            "🂴"

        Card Five Hearts ->
            "🂵"

        Card Six Hearts ->
            "🂶"

        Card Seven Hearts ->
            "🂷"

        Card Eight Hearts ->
            "🂸"

        Card Nine Hearts ->
            "🂹"

        Card Ten Hearts ->
            "🂺"

        Card Jack Hearts ->
            "🂻"

        Card Queen Hearts ->
            "🂽"

        Card King Hearts ->
            "🂾"

        Card Ace Diamonds ->
            "🃁"

        Card Two Diamonds ->
            "🃂"

        Card Three Diamonds ->
            "🃃"

        Card Four Diamonds ->
            "🃄"

        Card Five Diamonds ->
            "🃅"

        Card Six Diamonds ->
            "🃆"

        Card Seven Diamonds ->
            "🃇"

        Card Eight Diamonds ->
            "🃈"

        Card Nine Diamonds ->
            "🃉"

        Card Ten Diamonds ->
            "🃊"

        Card Jack Diamonds ->
            "🃋"

        Card Queen Diamonds ->
            "🃍"

        Card King Diamonds ->
            "🃎"

        Card Ace Clubs ->
            "🃑"

        Card Two Clubs ->
            "🃒"

        Card Three Clubs ->
            "🃓"

        Card Four Clubs ->
            "🃔"

        Card Five Clubs ->
            "🃕"

        Card Six Clubs ->
            "🃖"

        Card Seven Clubs ->
            "🃗"

        Card Eight Clubs ->
            "🃘"

        Card Nine Clubs ->
            "🃙"

        Card Ten Clubs ->
            "🃚"

        Card Jack Clubs ->
            "🃛"

        Card Queen Clubs ->
            "🃝"

        Card King Clubs ->
            "🃞"


stringCard : Card -> String
stringCard card =
    case card of
        Card Ace Spades ->
            "Card-Ace-Spades"

        Card Two Spades ->
            "Card-Two-Spades"

        Card Three Spades ->
            "Card-Three-Spades"

        Card Four Spades ->
            "Card-Four-Spades"

        Card Five Spades ->
            "Card-Five-Spades"

        Card Six Spades ->
            "Card-Six-Spades"

        Card Seven Spades ->
            "Card-Seven-Spades"

        Card Eight Spades ->
            "Card-Eight-Spades"

        Card Nine Spades ->
            "Card-Nine-Spades"

        Card Ten Spades ->
            "Card-Ten-Spades"

        Card Jack Spades ->
            "Card-Jack-Spades"

        Card Queen Spades ->
            "Card-Queen-Spades"

        Card King Spades ->
            "Card-King-Spades"

        Card Ace Hearts ->
            "Card-Ace-Hearts"

        Card Two Hearts ->
            "Card-Two-Hearts"

        Card Three Hearts ->
            "Card-Three-Hearts"

        Card Four Hearts ->
            "Card-Four-Hearts"

        Card Five Hearts ->
            "Card-Five-Hearts"

        Card Six Hearts ->
            "Card-Six-Hearts"

        Card Seven Hearts ->
            "Card-Seven-Hearts"

        Card Eight Hearts ->
            "Card-Eight-Hearts"

        Card Nine Hearts ->
            "Card-Nine-Hearts"

        Card Ten Hearts ->
            "Card-Ten-Hearts"

        Card Jack Hearts ->
            "Card-Jack-Hearts"

        Card Queen Hearts ->
            "Card-Queen-Hearts"

        Card King Hearts ->
            "Card-King-Hearts"

        Card Ace Diamonds ->
            "Card-Ace-Diamonds"

        Card Two Diamonds ->
            "Card-Two-Diamonds"

        Card Three Diamonds ->
            "Card-Three-Diamonds"

        Card Four Diamonds ->
            "Card-Four-Diamonds"

        Card Five Diamonds ->
            "Card-Five-Diamonds"

        Card Six Diamonds ->
            "Card-Six-Diamonds"

        Card Seven Diamonds ->
            "Card-Seven-Diamonds"

        Card Eight Diamonds ->
            "Card-Eight-Diamonds"

        Card Nine Diamonds ->
            "Card-Nine-Diamonds"

        Card Ten Diamonds ->
            "Card-Ten-Diamonds"

        Card Jack Diamonds ->
            "Card-Jack-Diamonds"

        Card Queen Diamonds ->
            "Card-Queen-Diamonds"

        Card King Diamonds ->
            "Card-King-Diamonds"

        Card Ace Clubs ->
            "Card-Ace-Clubs"

        Card Two Clubs ->
            "Card-Two-Clubs"

        Card Three Clubs ->
            "Card-Three-Clubs"

        Card Four Clubs ->
            "Card-Four-Clubs"

        Card Five Clubs ->
            "Card-Five-Clubs"

        Card Six Clubs ->
            "Card-Six-Clubs"

        Card Seven Clubs ->
            "Card-Seven-Clubs"

        Card Eight Clubs ->
            "Card-Eight-Clubs"

        Card Nine Clubs ->
            "Card-Nine-Clubs"

        Card Ten Clubs ->
            "Card-Ten-Clubs"

        Card Jack Clubs ->
            "Card-Jack-Clubs"

        Card Queen Clubs ->
            "Card-Queen-Clubs"

        Card King Clubs ->
            "Card-King-Clubs"


cardComp : Card -> Card -> Order
cardComp x y =
    case ( x, y ) of
        ( Card v1 s1, Card v2 s2 ) ->
            suiteOrdering s1 s2
                |> Ordering.ifStillTiedThen
                    (valueOrdering v1 v2)


valueRank : Value -> Int
valueRank v =
    case v of
        Ace ->
            1
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13



suiteRank s =
    case s of
        Clubs ->
            1

        Hearts ->
            2

        Diamonds ->
            3

        Spades ->
            4


cardRank card =
    case card of
        Card v s ->
            valueRank v + 13 * suiteRank s


cardOrdering =
    Ordering.byRank cardRank cardComp


suiteOrdering : Ordering Suite
suiteOrdering =
    Ordering.explicit
        [ Clubs, Hearts, Diamonds, Spades ]


valueOrdering : Ordering Value
valueOrdering =
    Ordering.explicit
        [ Ace
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Ten
        , Jack
        , Queen
        , King
        ]


cardsQuickSort : List Card -> List Card
cardsQuickSort =
    quickSort cardRank


orderdDeck =
    [ Card Ace Clubs
    , Card Two Clubs
    , Card Three Clubs
    , Card Four Clubs
    , Card Five Clubs
    , Card Six Clubs
    , Card Seven Clubs
    , Card Eight Clubs
    , Card Nine Clubs
    , Card Ten Clubs
    , Card Jack Clubs
    , Card Queen Clubs
    , Card King Clubs
    , Card Ace Hearts
    , Card Two Hearts
    , Card Three Hearts
    , Card Four Hearts
    , Card Five Hearts
    , Card Six Hearts
    , Card Seven Hearts
    , Card Eight Hearts
    , Card Nine Hearts
    , Card Ten Hearts
    , Card Jack Hearts
    , Card Queen Hearts
    , Card King Hearts
    , Card Ace Diamonds
    , Card Two Diamonds
    , Card Three Diamonds
    , Card Four Diamonds
    , Card Five Diamonds
    , Card Six Diamonds
    , Card Seven Diamonds
    , Card Eight Diamonds
    , Card Nine Diamonds
    , Card Ten Diamonds
    , Card Jack Diamonds
    , Card Queen Diamonds
    , Card King Diamonds
    , Card Ace Spades
    , Card Two Spades
    , Card Three Spades
    , Card Four Spades
    , Card Five Spades
    , Card Six Spades
    , Card Seven Spades
    , Card Eight Spades
    , Card Nine Spades
    , Card Ten Spades
    , Card Jack Spades
    , Card Queen Spades
    , Card King Spades
    ]


type alias Deck =
    List Card


newDeck : Deck
newDeck =
    let
        ( d, _ ) =
            Random.step (Random.List.shuffle orderdDeck) (Random.initialSeed 0)
    in
    d


colorCard : Card -> String
colorCard c =
    case c of
        Card _ Hearts ->
            "red"

        Card _ Diamonds ->
            "red"

        Card _ Spades ->
            "black"

        Card _ Clubs ->
            "black"
