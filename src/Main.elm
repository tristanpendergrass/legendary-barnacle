module Main exposing (main)

import Browser
import FightingCard
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (class, type_)
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


type DecidingHazardState
    = TwoOptions FightingCard.FightingCard FightingCard.FightingCard
    | OneOption FightingCard.FightingCard


type GameState
    = DecidingHazard CommonGameState DecidingHazardState
    | FightingHazard CommonGameState
    | ResolvingFight CommonGameState
    | FinalShowdown CommonGameState


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

        ( ( ( leftHazard, rightHazard ), hazardCards ), seedAfterHazardShuffle ) =
            Random.step FightingCard.getInitialHazardCards seedAfterRobinsonShuffle

        ( ( leftPirate, rightPirate ), seedAfterPirateShuffle ) =
            Random.step PirateCard.getTwoPirates seedAfterHazardShuffle

        commonGameState : CommonGameState
        commonGameState =
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

        decidingHazardState : DecidingHazardState
        decidingHazardState =
            TwoOptions leftHazard rightHazard
    in
    ( GameInProgress (DecidingHazard commonGameState decidingHazardState), Cmd.none )



-- UPDATE


type Msg
    = EndGame
      -- DecidingHazard phase
    | ChooseLeftHazard
    | ChooseRightHazard
    | ChooseSingleHazard
    | ChooseSkipHazard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameInProgress gameState ->
            updateGameInProgress msg gameState

        GameOver ->
            ( model, Cmd.none )


updateGameInProgress : Msg -> GameState -> ( Model, Cmd Msg )
updateGameInProgress msg gameState =
    let
        noOp : ( Model, Cmd Msg )
        noOp =
            ( GameInProgress gameState, Cmd.none )
    in
    case ( msg, gameState ) of
        -- DecidingHazard phase
        ( ChooseLeftHazard, DecidingHazard commonGameState decidingHazardState ) ->
            case decidingHazardState of
                TwoOptions _ _ ->
                    ( GameInProgress (FightingHazard commonGameState), Cmd.none )

                OneOption _ ->
                    noOp

        ( ChooseRightHazard, DecidingHazard commonGameState decidingHazardState ) ->
            case decidingHazardState of
                TwoOptions _ _ ->
                    ( GameInProgress (FightingHazard commonGameState), Cmd.none )

                OneOption _ ->
                    noOp

        ( ChooseSingleHazard, DecidingHazard commonGameState decidingHazardState ) ->
            case decidingHazardState of
                TwoOptions _ _ ->
                    noOp

                OneOption _ ->
                    ( GameInProgress (FightingHazard commonGameState), Cmd.none )

        ( ChooseSkipHazard, DecidingHazard commonGameState decidingHazardState ) ->
            case decidingHazardState of
                TwoOptions _ _ ->
                    noOp

                OneOption _ ->
                    ( GameInProgress (FightingHazard commonGameState), Cmd.none )

        _ ->
            noOp



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        GameOver ->
            div [ class "m-6 text-xl" ] [ text "Game over" ]

        GameInProgress gameState ->
            case gameState of
                DecidingHazard _ (TwoOptions leftHazard rightHazard) ->
                    div []
                        [ div [ class "m-6 text-xl" ] [ text "Decide Hazard" ]
                        , div [ class "m-6 flex" ]
                            [ button [ class "inline-block mr-4" ] [ text "left" ]
                            , button [ class "inline-block mr-4" ] [ text "right" ]
                            ]
                        ]

                DecidingHazard _ (OneOption hazard) ->
                    div []
                        [ div [ class "m-6 text-xl" ] [ text "Decide Hazard" ]
                        , div [ class "m-6 flex" ]
                            [ button [ class "inline-block mr-4" ] [ text "Choose hazard" ]
                            , button [ class "inline-block mr-4" ] [ text "Skip hazard" ]
                            ]
                        ]

                _ ->
                    div [ class "m-6 text-xl" ] [ text "Stage not supported" ]
