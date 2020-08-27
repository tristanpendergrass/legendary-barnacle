module Main exposing (main)

import Browser
import FightingCard
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import LifePoints
import PirateCard
import Random
import Random.List


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Phase
    = PhaseGreen
    | PhaseYellow
    | PhaseRed


type alias CommonGameState =
    { lifePoints : LifePoints.Counter
    , phase : Phase
    , agingCards : List FightingCard.FightingCard
    , fightingCards : List FightingCard.FightingCard
    , hazardCards : List FightingCard.FightingCard
    , leftPirate : PirateCard.PirateCard
    , rightPirate : PirateCard.PirateCard
    , robinsonDiscard : List FightingCard.FightingCard
    , hazardDiscard : List FightingCard.FightingCard
    }


type GameState
    = DecidingHazard
    | FightingHazard
    | ResolvingFight
    | FinalShowdown


type Model
    = GameInProgress GameState
    | GameOver


init : () -> ( Model, Cmd Msg )
init _ =
    let
        --  TODO: Pull this from outside of program
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed 0

        ( agingCards, seedAfterAgingShuffle ) =
            Random.step FightingCard.getInitialAgingCards initialSeed

        ( robinsonCards, seedAfterRobinsonShuffle ) =
            Random.step FightingCard.getInitialRobinsonCards seedAfterAgingShuffle

        ( hazardCards, seedAfterHazardShuffle ) =
            Random.step FightingCard.getInitialHazardCards seedAfterRobinsonShuffle

        ( ( leftPirate, rightPirate ), seedAfterPirateShuffle ) =
            Random.step PirateCard.getTwoPirates seedAfterHazardShuffle

        initialGameState : GameState
        initialGameState =
            { lifePoints = LifePoints.createCounter 20
            , phase = PhaseGreen
            , agingCards = agingCards
            , fightingCards = robinsonCards
            , hazardCards = hazardCards
            , leftPirate = leftPirate
            , rightPirate = rightPirate
            , robinsonDiscard = []
            , hazardDiscard = []
            }
    in
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = HandleThingInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "m-6 text-xl" ] [ text "Why hello there." ]
