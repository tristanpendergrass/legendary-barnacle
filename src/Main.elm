module Main exposing (main)

import Browser
import Card exposing (Card)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import LifePoints
import PirateCard exposing (PirateCard)
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


type alias CommonState =
    { seed : Random.Seed
    , lifePoints : LifePoints.Counter
    , phase : Phase
    , agingCards : List Card
    , fightingCards : List Card
    , hazardCards : List Card
    , leftPirate : PirateCard
    , rightPirate : PirateCard
    , robinsonDiscard : List Card
    , hazardDiscard : List Card
    }


type OneOrTwo a
    = Two a a
    | One a


type GameState
    = HazardSelection CommonState (OneOrTwo Card)
    | FightingHazard CommonState
    | ResolvingFight CommonState
    | FinalShowdown CommonState


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
            Random.step Card.getInitialAgingCards initialSeed

        ( robinsonCards, seedAfterRobinsonShuffle ) =
            Random.step Card.getInitialRobinsonCards seedAfterAgingShuffle

        ( ( ( leftHazard, rightHazard ), hazardCards ), seedAfterHazardShuffle ) =
            Random.step Card.getInitialHazardCards seedAfterRobinsonShuffle

        ( ( leftPirate, rightPirate ), seedAfterPirateShuffle ) =
            Random.step PirateCard.getTwoPirates seedAfterHazardShuffle

        commonState : CommonState
        commonState =
            { seed = seedAfterPirateShuffle
            , lifePoints = LifePoints.createCounter 20
            , phase = PhaseGreen
            , agingCards = agingCards
            , fightingCards = robinsonCards
            , hazardCards = hazardCards
            , leftPirate = leftPirate
            , rightPirate = rightPirate
            , robinsonDiscard = []
            , hazardDiscard = []
            }

        hazardSelectionState : OneOrTwo Card
        hazardSelectionState =
            Two leftHazard rightHazard
    in
    ( GameInProgress (HazardSelection commonState hazardSelectionState), Cmd.none )



-- UPDATE


type Msg
    = EndGame
      -- Hazard selection phase
    | ChooseLeftHazard
    | ChooseRightHazard
    | ChooseSingleHazard
    | ChooseSkipHazard


discardOneOrTwoHazards : OneOrTwo Card -> CommonState -> CommonState
discardOneOrTwoHazards oneOrTwo commonState =
    { commonState
        | hazardDiscard =
            case oneOrTwo of
                One card ->
                    card :: commonState.hazardDiscard

                Two first second ->
                    first :: second :: commonState.hazardDiscard
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameInProgress gameState ->
            updateGameInProgress msg gameState

        GameOver ->
            ( model, Cmd.none )


{-| Returns the game state to hazard selection with the phase updated, or moves to the final showdown if already in PhaseRed
-}
toNextPhase : OneOrTwo Card -> CommonState -> GameState
toNextPhase leftoverCards incompleteCommonState =
    let
        commonState : CommonState
        commonState =
            discardOneOrTwoHazards leftoverCards incompleteCommonState

        toHazardSelection : Phase -> GameState
        toHazardSelection phase =
            let
                ( shuffledHazards, newSeed ) =
                    Random.step (Random.List.shuffle commonState.hazardDiscard) commonState.seed
            in
            case shuffledHazards of
                [] ->
                    HazardSelection { commonState | phase = phase } leftoverCards

                [ singleCard ] ->
                    HazardSelection { commonState | phase = phase, hazardDiscard = [], seed = newSeed } (One singleCard)

                first :: second :: rest ->
                    HazardSelection { commonState | phase = phase, hazardDiscard = rest, seed = newSeed } (Two first second)
    in
    case commonState.phase of
        PhaseRed ->
            FinalShowdown commonState

        PhaseYellow ->
            toHazardSelection PhaseRed

        PhaseGreen ->
            toHazardSelection PhaseYellow


updateGameInProgress : Msg -> GameState -> ( Model, Cmd Msg )
updateGameInProgress msg gameState =
    let
        noOp : ( Model, Cmd Msg )
        noOp =
            ( GameInProgress gameState, Cmd.none )
    in
    case ( msg, gameState ) of
        -- Hazard Selection phase
        ( ChooseLeftHazard, HazardSelection commonState (Two _ _) ) ->
            ( GameInProgress (FightingHazard commonState), Cmd.none )

        ( ChooseRightHazard, HazardSelection commonState (Two _ _) ) ->
            ( GameInProgress (FightingHazard commonState), Cmd.none )

        ( ChooseSingleHazard, HazardSelection commonState (One _) ) ->
            ( GameInProgress (FightingHazard commonState), Cmd.none )

        ( ChooseSkipHazard, HazardSelection commonState (One card) ) ->
            ( GameInProgress (toNextPhase (One card) commonState), Cmd.none )

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
                HazardSelection _ (Two leftHazard rightHazard) ->
                    div []
                        [ div [ class "m-6 text-xl" ] [ text "Decide Hazard" ]
                        , div [ class "m-6 flex" ]
                            [ button [ class "inline-block mr-4", onClick ChooseLeftHazard ] [ text "left" ]
                            , button [ class "inline-block mr-4", onClick ChooseRightHazard ] [ text "right" ]
                            ]
                        ]

                HazardSelection _ (One hazard) ->
                    div []
                        [ div [ class "m-6 text-xl" ] [ text "Decide Hazard" ]
                        , div [ class "m-6 flex" ]
                            [ button [ class "inline-block mr-4", onClick ChooseSingleHazard ] [ text "Choose hazard" ]
                            , button [ class "inline-block mr-4", onClick ChooseSkipHazard ] [ text "Skip hazard" ]
                            ]
                        ]

                _ ->
                    div [ class "m-6 text-xl" ] [ text "Stage not supported" ]
