module Main exposing (main)

import AgingCard exposing (AgingCard)
import Browser
import FightArea exposing (FightArea)
import FightStats exposing (SpecialAbility(..))
import HazardCard exposing (HazardCard)
import HazardDeck exposing (HazardDeck)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import LifePoints
import Phase exposing (Phase(..))
import PirateCard exposing (PirateCard)
import PlayerCard exposing (PlayerCard)
import PlayerDeck exposing (PlayerDeck)
import Random
import Random.List
import RobinsonCard exposing (RobinsonCard)
import SelectionList exposing (SelectionList)
import SortArea exposing (SortArea)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias CommonState =
    { seed : Random.Seed
    , lifePoints : LifePoints.Counter
    , phase : Phase
    , pirateOne : PirateCard
    , pirateTwo : PirateCard
    , hazardDeck : HazardDeck
    , playerDeck : PlayerDeck
    }


type PirateState
    = FirstPirate
    | SecondPirate


type OneOrTwo a
    = Two a a
    | One a


type ResolvingState
    = PlayerWon HazardCard
    | PlayerLost Int (SelectionList PlayerCard)


type FightView
    = NormalFightView
    | SortView (SortArea PlayerCard)


type GameState
    = HazardSelection CommonState (OneOrTwo HazardCard)
    | FightingHazard CommonState (FightArea HazardCard) FightView
    | ResolvingFight CommonState ResolvingState
    | FinalShowdown CommonState PirateState (FightArea PirateCard) FightView


type Model
    = GameInProgress GameState
    | GameOver


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

        playerDeck : PlayerDeck
        playerDeck =
            robinsonCards
                |> List.map PlayerCard.fromRobinsonCard
                |> PlayerDeck.create agingCards

        ( hazardOne, hazardTwo, remainingHazards ) =
            HazardCard.getInitial

        ( ( leftHazard, rightHazard, hazardCards ), seedAfterHazardShuffle ) =
            Random.step (addTwoShuffleAndDraw hazardOne hazardTwo remainingHazards) seedAfterRobinsonCards

        hazardDeck : HazardDeck
        hazardDeck =
            HazardDeck.create hazardCards

        ( ( pirateOne, pirateTwo ), seedAfterPirateShuffle ) =
            Random.step PirateCard.getTwoPirates seedAfterHazardShuffle

        commonState : CommonState
        commonState =
            { seed = seedAfterPirateShuffle
            , lifePoints = LifePoints.createCounter 20
            , phase = PhaseGreen
            , pirateOne = pirateOne
            , pirateTwo = pirateTwo
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
    | UseAbility Int
      -- case FightView of SortView
    | SortFinish
    | SortChangeOrder SortArea.ChangeOrderType
    | SortDiscard SortArea.SortIndex
    | SortReveal
      -- Resolving hazard
    | AcceptWin
    | ToggleLossDestroy Int
    | AcceptLoss



-- Final Showdown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameInProgress gameState ->
            updateGameInProgress msg gameState

        GameOver ->
            ( model, Cmd.none )


toFinalShowdown : CommonState -> GameState
toFinalShowdown commonState =
    FinalShowdown
        commonState
        FirstPirate
        (FightArea.createFightArea commonState.pirateOne)
        NormalFightView


{-| Returns the game state to hazard selection with the phase updated, or moves to the final showdown if already in PhaseRed
-}
handlePhaseComplete : Maybe HazardCard -> CommonState -> GameState
handlePhaseComplete maybeHazardCard incompleteCommonState =
    -- TODO revisit this function; it is confusing and should be refactored if possible
    let
        leftoverCards : List HazardCard
        leftoverCards =
            case maybeHazardCard of
                Just card ->
                    [ card ]

                Nothing ->
                    []

        commonState : CommonState
        commonState =
            { incompleteCommonState | hazardDeck = HazardDeck.discard leftoverCards incompleteCommonState.hazardDeck }

        toHazardSelection : Phase -> GameState
        toHazardSelection phase =
            let
                ( shuffledHazardDeck, newSeed ) =
                    Random.step (HazardDeck.reshuffle commonState.hazardDeck) commonState.seed

                drawTwiceResult : HazardDeck.DrawTwiceResult
                drawTwiceResult =
                    HazardDeck.drawTwice shuffledHazardDeck
            in
            case drawTwiceResult of
                HazardDeck.NothingDrawn ->
                    -- Should never happen that all hazards run out, but if it does move directly to final showdown
                    toFinalShowdown { commonState | phase = PhaseRed }

                HazardDeck.DrewOne newHazardDeck hazardCard ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = newSeed } (One hazardCard)

                HazardDeck.DrewTwo newHazardDeck first second ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = newSeed } (Two first second)
    in
    case commonState.phase of
        PhaseRed ->
            toFinalShowdown { commonState | phase = PhaseRed }

        PhaseYellow ->
            toHazardSelection PhaseRed

        PhaseGreen ->
            toHazardSelection PhaseYellow


toFightingHazard : HazardCard -> CommonState -> GameState
toFightingHazard hazard commonState =
    FightingHazard commonState (FightArea.createFightArea hazard) NormalFightView


drawCard : CommonState -> Maybe ( PlayerCard, CommonState )
drawCard commonState =
    let
        { playerDeck, seed } =
            commonState
    in
    case Random.step (PlayerDeck.draw playerDeck) seed of
        ( Nothing, _ ) ->
            Nothing

        ( Just ( drawnCard, newPlayerDeck ), newSeed ) ->
            Just ( drawnCard, { commonState | playerDeck = newPlayerDeck, seed = newSeed } )


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
            ( GameInProgress (toFightingHazard left { commonState | hazardDeck = HazardDeck.discard [ right ] commonState.hazardDeck }), Cmd.none )

        ( ChooseRightHazard, HazardSelection commonState (Two left right) ) ->
            ( GameInProgress (toFightingHazard right { commonState | hazardDeck = HazardDeck.discard [ left ] commonState.hazardDeck }), Cmd.none )

        ( ChooseSingleHazard, HazardSelection commonState (One hazard) ) ->
            ( GameInProgress (toFightingHazard hazard commonState), Cmd.none )

        ( ChooseSkipHazard, HazardSelection commonState (One card) ) ->
            ( GameInProgress (handlePhaseComplete (Just card) commonState), Cmd.none )

        -- Fight
        ( Draw, FightingHazard commonState fightArea NormalFightView ) ->
            case drawCard commonState of
                Nothing ->
                    noOp

                Just ( drawnCard, newCommonState ) ->
                    let
                        newFightArea : FightArea HazardCard
                        newFightArea =
                            FightArea.playCard drawnCard fightArea
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
                    FightArea.getEnemy fightArea

                hazardStrength : Int
                hazardStrength =
                    case phase of
                        PhaseGreen ->
                            HazardCard.getGreenValue hazard

                        PhaseYellow ->
                            HazardCard.getYellowValue hazard

                        PhaseRed ->
                            HazardCard.getRedValue hazard

                strengthDifference : Int
                strengthDifference =
                    hazardStrength - playerStrength

                playerWon : Bool
                playerWon =
                    strengthDifference <= 0
            in
            if playerWon then
                let
                    discardedCards : List PlayerCard
                    discardedCards =
                        FightArea.getCards fightArea

                    newPlayerDeck : PlayerDeck
                    newPlayerDeck =
                        PlayerDeck.discard discardedCards playerDeck

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | playerDeck = newPlayerDeck }

                    resolvingState : ResolvingState
                    resolvingState =
                        PlayerWon hazard
                in
                ( GameInProgress (ResolvingFight newCommonState resolvingState), Cmd.none )

            else
                -- Player lost, strengthDifference > 0
                case LifePoints.decrementCounter strengthDifference commonState.lifePoints of
                    Nothing ->
                        ( GameOver, Cmd.none )

                    Just newLifePoints ->
                        let
                            newHazardDeck : HazardDeck
                            newHazardDeck =
                                HazardDeck.discard [ hazard ] hazardDeck

                            newCommonState : CommonState
                            newCommonState =
                                { commonState | hazardDeck = newHazardDeck, lifePoints = newLifePoints }

                            playerCardList : SelectionList PlayerCard
                            playerCardList =
                                FightArea.getCards fightArea
                                    |> SelectionList.create strengthDifference

                            resolvingState : ResolvingState
                            resolvingState =
                                PlayerLost strengthDifference playerCardList
                        in
                        ( GameInProgress (ResolvingFight newCommonState resolvingState), Cmd.none )

        ( UseAbility index, FightingHazard commonState fightArea NormalFightView ) ->
            case FightArea.attemptUse index fightArea of
                Just attemptUseResult ->
                    let
                        ( ability, { setCardInUse, setCardUsed } ) =
                            attemptUseResult

                        newGameState =
                            resolveAbility
                                { ability = ability
                                , setCardInUse = setCardInUse
                                , setCardUsed = setCardUsed
                                , commonState = commonState
                                , fightArea = fightArea
                                }
                    in
                    ( GameInProgress newGameState, Cmd.none )

                Nothing ->
                    noOp

        ( SortFinish, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                { cardsToKeep, cardsToDiscard } =
                    SortArea.getCards sortArea

                newPlayerDeck : PlayerDeck
                newPlayerDeck =
                    commonState.playerDeck
                        |> PlayerDeck.putOnTop cardsToKeep
                        |> PlayerDeck.discard cardsToDiscard

                newCommonState : CommonState
                newCommonState =
                    { commonState | playerDeck = newPlayerDeck }

                newFightArea : FightArea HazardCard
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
            case ( SortArea.attemptReveal sortArea, drawCard commonState ) of
                ( Just onReveal, Just ( drawnCard, newCommonState ) ) ->
                    let
                        newSortArea : SortArea PlayerCard
                        newSortArea =
                            onReveal drawnCard
                    in
                    ( GameInProgress (FightingHazard newCommonState fightArea (SortView newSortArea)), Cmd.none )

                _ ->
                    noOp

        ( AcceptWin, ResolvingFight commonState (PlayerWon hazardCard) ) ->
            let
                newCommonState : CommonState
                newCommonState =
                    { commonState | playerDeck = PlayerDeck.discard [ PlayerCard.fromHazardCard hazardCard ] commonState.playerDeck }
            in
            case HazardDeck.drawTwice commonState.hazardDeck of
                HazardDeck.NothingDrawn ->
                    ( GameInProgress (handlePhaseComplete Nothing newCommonState), Cmd.none )

                HazardDeck.DrewOne newHazardDeck card ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (One card)), Cmd.none )

                HazardDeck.DrewTwo newHazardDeck first second ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (Two first second)), Cmd.none )

        ( ToggleLossDestroy index, ResolvingFight commonState (PlayerLost lifeLost selectionList) ) ->
            case SelectionList.attemptToggle index selectionList of
                Nothing ->
                    noOp

                Just newSelectionList ->
                    ( GameInProgress (ResolvingFight commonState (PlayerLost lifeLost newSelectionList)), Cmd.none )

        ( AcceptLoss, ResolvingFight commonState (PlayerLost _ selectionList) ) ->
            let
                newCommonState : CommonState
                newCommonState =
                    { commonState | playerDeck = PlayerDeck.discard (SelectionList.getUnselected selectionList) commonState.playerDeck }
            in
            case HazardDeck.drawTwice commonState.hazardDeck of
                -- TODO: check if this logic is exactly the same as some other spot, refactor into a function
                HazardDeck.NothingDrawn ->
                    ( GameInProgress (handlePhaseComplete Nothing newCommonState), Cmd.none )

                HazardDeck.DrewOne newHazardDeck card ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (One card)), Cmd.none )

                HazardDeck.DrewTwo newHazardDeck first second ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (Two first second)), Cmd.none )

        _ ->
            noOp


type alias ResolveAbilityArg a =
    { ability : SpecialAbility
    , setCardInUse : FightArea a
    , setCardUsed : FightArea a
    , commonState : CommonState
    , fightArea : FightArea a
    }


resolveAbility : ResolveAbilityArg HazardCard -> GameState
resolveAbility { ability, setCardInUse, setCardUsed, commonState, fightArea } =
    case ability of
        PlusOneLife ->
            let
                newLifePoints : LifePoints.Counter
                newLifePoints =
                    LifePoints.incrementCounter commonState.lifePoints

                newCommonState : CommonState
                newCommonState =
                    { commonState | lifePoints = newLifePoints }

                newFightArea : FightArea HazardCard
                newFightArea =
                    setCardUsed
            in
            FightingHazard newCommonState newFightArea NormalFightView

        SortThree ->
            case drawCard commonState of
                Nothing ->
                    FightingHazard commonState fightArea NormalFightView

                Just ( drawnCard, newCommonState ) ->
                    FightingHazard newCommonState setCardInUse (SortView (SortArea.create drawnCard))

        _ ->
            Debug.todo "Implement missing ability"



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
