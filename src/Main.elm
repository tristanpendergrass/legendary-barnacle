module Main exposing (main)

import AgingCard exposing (AgingCard)
import Browser
import Deck exposing (Deck)
import FightArea exposing (FightArea)
import HazardCard exposing (HazardCard)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import LifePoints
import Phase exposing (Phase(..))
import PirateCard exposing (PirateCard)
import PlayerCard exposing (PlayerCard)
import Random
import Random.List
import RobinsonCard exposing (RobinsonCard)
import SortArea exposing (SortArea)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias CommonState =
    { seed : Random.Seed
    , lifePoints : LifePoints.Counter
    , phase : Phase
    , agingCards : List AgingCard
    , leftPirate : PirateCard
    , rightPirate : PirateCard
    , hazardDeck : Deck HazardCard
    , playerDeck : Deck PlayerCard
    }


type OneOrTwo a
    = Two a a
    | One a


type ResolvingState
    = PlayerWon HazardCard
    | PlayerLost (List PlayerCard) (List PlayerCard)


type FightView
    = NormalFightView
    | SortView (SortArea PlayerCard)


type GameState
    = HazardSelection CommonState (OneOrTwo HazardCard)
    | FightingHazard CommonState FightArea FightView
    | ResolvingFight CommonState ResolvingState
    | FinalShowdown CommonState


type Model
    = GameInProgress GameState
    | GameOver


discardOneOrTwo : OneOrTwo a -> Deck a -> Deck a
discardOneOrTwo oneOrTwo deck =
    case oneOrTwo of
        One card ->
            Deck.discard [ card ] deck

        Two first second ->
            Deck.discard [ first, second ] deck


addTwoShuffleAndDraw : a -> a -> List a -> Random.Generator ( a, a, List a )
addTwoShuffleAndDraw first second rest =
    (first :: second :: rest)
        |> Random.List.shuffle
        |> Random.map
            (\shuffledCards ->
                case shuffledCards of
                    topCard :: secondCard :: restShuffled ->
                        ( topCard, secondCard, restShuffled )

                    _ ->
                        ( first, second, rest )
            )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        --  TODO: Pull this from outside of program
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed 0

        ( agingCards, seedAfterAgingShuffle ) =
            Random.step AgingCard.getInitial initialSeed

        ( robinsonCards, seedAfterRobinsonCards ) =
            Random.step (Random.List.shuffle RobinsonCard.getInitial) seedAfterAgingShuffle

        playerDeck : Deck PlayerCard
        playerDeck =
            robinsonCards
                |> List.map PlayerCard.fromRobinsonCard
                |> Deck.createDeck

        ( hazardOne, hazardTwo, remainingHazards ) =
            HazardCard.getInitial

        ( ( leftHazard, rightHazard, hazardCards ), seedAfterHazardShuffle ) =
            Random.step (addTwoShuffleAndDraw hazardOne hazardTwo remainingHazards) seedAfterRobinsonCards

        hazardDeck : Deck HazardCard
        hazardDeck =
            remainingHazards
                |> Deck.createDeck

        ( ( leftPirate, rightPirate ), seedAfterPirateShuffle ) =
            Random.step PirateCard.getTwoPirates seedAfterHazardShuffle

        commonState : CommonState
        commonState =
            { seed = seedAfterPirateShuffle
            , lifePoints = LifePoints.createCounter 20
            , phase = PhaseGreen
            , agingCards = agingCards
            , leftPirate = leftPirate
            , rightPirate = rightPirate
            , playerDeck = playerDeck
            , hazardDeck = hazardDeck
            }

        hazardSelectionState : OneOrTwo HazardCard
        hazardSelectionState =
            Two leftHazard rightHazard
    in
    ( GameInProgress (HazardSelection commonState hazardSelectionState), Cmd.none )



-- UPDATE


type Msg
    = EndGame
      -- Hazard selection
    | ChooseLeftHazard
    | ChooseRightHazard
    | ChooseSingleHazard
    | ChooseSkipHazard
      -- Fighting hazard
      -- case FightView of NormalView
    | Draw
    | EndFight
    | UseAbility FightArea.FightAreaIndex
      -- case FightView of SortView
    | SortFinish
    | SortChangeOrder SortArea.ChangeOrderType
    | SortDiscard SortArea.DiscardType
    | SortReveal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameInProgress gameState ->
            updateGameInProgress msg gameState

        GameOver ->
            ( model, Cmd.none )


{-| Returns the game state to hazard selection with the phase updated, or moves to the final showdown if already in PhaseRed
-}
handlePhaseComplete : OneOrTwo HazardCard -> CommonState -> GameState
handlePhaseComplete leftoverCards incompleteCommonState =
    let
        commonState : CommonState
        commonState =
            { incompleteCommonState | hazardDeck = discardOneOrTwo leftoverCards incompleteCommonState.hazardDeck }

        toHazardSelection : Phase -> GameState
        toHazardSelection phase =
            let
                ( shuffledHazards, seedAfterReshuffle ) =
                    Random.step (Deck.reshuffle commonState.hazardDeck) commonState.seed

                drawTwiceResultAndSeed : ( Deck.DrawTwiceResult HazardCard, Random.Seed )
                drawTwiceResultAndSeed =
                    Random.step (Deck.drawTwice shuffledHazards) seedAfterReshuffle

                ( drawTwiceResult, seedAfterDraw ) =
                    drawTwiceResultAndSeed
            in
            case drawTwiceResult of
                Deck.NothingDrawn ->
                    HazardSelection { commonState | phase = phase } leftoverCards

                Deck.DrewOne newHazardDeck hazardCard ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = seedAfterDraw } (One hazardCard)

                Deck.DrewTwo newHazardDeck first second ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = seedAfterDraw } (Two first second)
    in
    case commonState.phase of
        PhaseRed ->
            FinalShowdown commonState

        PhaseYellow ->
            toHazardSelection PhaseRed

        PhaseGreen ->
            toHazardSelection PhaseYellow


toFightingHazard : HazardCard -> CommonState -> GameState
toFightingHazard hazard commonState =
    FightingHazard commonState (FightArea.createFightArea hazard) NormalFightView


updateGameInProgress : Msg -> GameState -> ( Model, Cmd Msg )
updateGameInProgress msg gameState =
    let
        noOp : ( Model, Cmd Msg )
        noOp =
            ( GameInProgress gameState, Cmd.none )
    in
    case ( msg, gameState ) of
        -- HazardSelection
        ( ChooseLeftHazard, HazardSelection commonState (Two left right) ) ->
            ( GameInProgress (toFightingHazard left { commonState | hazardDeck = Deck.discard [ right ] commonState.hazardDeck }), Cmd.none )

        ( ChooseRightHazard, HazardSelection commonState (Two left right) ) ->
            ( GameInProgress (toFightingHazard right { commonState | hazardDeck = Deck.discard [ left ] commonState.hazardDeck }), Cmd.none )

        ( ChooseSingleHazard, HazardSelection commonState (One hazard) ) ->
            ( GameInProgress (toFightingHazard hazard commonState), Cmd.none )

        ( ChooseSkipHazard, HazardSelection commonState (One card) ) ->
            ( GameInProgress (handlePhaseComplete (One card) commonState), Cmd.none )

        -- Fight
        ( Draw, FightingHazard commonState fightArea NormalFightView ) ->
            let
                { playerDeck, seed } =
                    commonState
            in
            case Random.step (Deck.draw playerDeck) seed of
                ( Nothing, _ ) ->
                    noOp

                ( Just ( drawnCard, newPlayerDeck ), newSeed ) ->
                    let
                        newCommonState : CommonState
                        newCommonState =
                            { commonState | seed = newSeed, playerDeck = newPlayerDeck }

                        newFightArea : FightArea
                        newFightArea =
                            if FightArea.canDrawFreeCard fightArea then
                                FightArea.playOnLeft drawnCard fightArea

                            else
                                FightArea.playOnRight drawnCard fightArea
                    in
                    ( GameInProgress (FightingHazard newCommonState newFightArea NormalFightView), Cmd.none )

        ( EndFight, FightingHazard commonState fightArea NormalFightView ) ->
            let
                { phase, playerDeck, hazardDeck } =
                    commonState

                playerStrength : Int
                playerStrength =
                    FightArea.getPlayerStrength fightArea

                hazard : HazardCard
                hazard =
                    FightArea.getHazard fightArea

                hazardStrength : Int
                hazardStrength =
                    FightArea.getHazardStrength phase fightArea

                playerWon : Bool
                playerWon =
                    playerStrength >= hazardStrength
            in
            if playerWon then
                let
                    discardedCards : List PlayerCard
                    discardedCards =
                        List.concat [ FightArea.getLeftCards fightArea, FightArea.getRightCards fightArea ]

                    newPlayerDeck : Deck PlayerCard
                    newPlayerDeck =
                        Deck.discard discardedCards playerDeck

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | playerDeck = newPlayerDeck }

                    resolvingState : ResolvingState
                    resolvingState =
                        PlayerWon hazard
                in
                ( GameInProgress (ResolvingFight newCommonState resolvingState), Cmd.none )

            else
                let
                    newHazardDeck : Deck HazardCard
                    newHazardDeck =
                        Deck.discard [ hazard ] hazardDeck

                    newLifePoints : LifePoints.Counter
                    newLifePoints =
                        LifePoints.decrementCounter (hazardStrength - playerStrength) commonState.lifePoints

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | hazardDeck = newHazardDeck, lifePoints = newLifePoints }

                    resolvingState : ResolvingState
                    resolvingState =
                        PlayerLost (FightArea.getLeftCards fightArea) (FightArea.getRightCards fightArea)
                in
                ( GameInProgress (ResolvingFight newCommonState resolvingState), Cmd.none )

        ( UseAbility fightAreaIndex, FightingHazard commonState fightArea NormalFightView ) ->
            if FightArea.cardCanBeActivated fightAreaIndex fightArea then
                let
                    newFightArea : FightArea
                    newFightArea =
                        FightArea.setCardUsed fightAreaIndex fightArea

                    newLifePoints : LifePoints.Counter
                    newLifePoints =
                        LifePoints.incrementCounter commonState.lifePoints

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | lifePoints = newLifePoints }
                in
                ( GameInProgress (FightingHazard newCommonState newFightArea NormalFightView), Cmd.none )

            else
                noOp

        ( SortFinish, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                { cardsToKeep, cardsToDiscard } =
                    SortArea.getCards sortArea

                newPlayerDeck : Deck PlayerCard
                newPlayerDeck =
                    commonState.playerDeck
                        |> Deck.putOnDrawPile cardsToKeep
                        |> Deck.discard cardsToDiscard

                newCommonState : CommonState
                newCommonState =
                    { commonState | playerDeck = newPlayerDeck }

                newFightArea : FightArea
                newFightArea =
                    FightArea.setInUseToUsed fightArea
            in
            ( GameInProgress (FightingHazard newCommonState newFightArea NormalFightView), Cmd.none )

        ( SortChangeOrder changeOrderType, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.changeOrder changeOrderType sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea (SortView newSortArea)), Cmd.none )

        ( SortDiscard discardType, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.toggleDiscard discardType sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea (SortView newSortArea)), Cmd.none )

        ( SortReveal, FightingHazard commonState fightArea (SortView sortArea) ) ->
            noOp

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
