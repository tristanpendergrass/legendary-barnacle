module Main exposing (main)

import Ability exposing (Ability)
import AgingCard exposing (AgingCard)
import Browser
import FightArea exposing (FightArea)
import FightStats exposing (SpecialAbility(..))
import HazardCard exposing (HazardCard)
import HazardDeck exposing (HazardDeck)
import Html exposing (Html, a, button, div, h1, h2, h3, img, li, span, text, ul)
import Html.Attributes exposing (class, disabled, src)
import Html.Events exposing (onClick)
import LifePoints
import Phase exposing (Phase(..))
import PirateCard exposing (PirateCard)
import PlayerCard exposing (PlayerCard)
import PlayerDeck exposing (PlayerDeck)
import Random
import Random.List
import Result.Extra
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
    , pirateStatus : PirateStatus
    , hazardDeck : HazardDeck
    , playerDeck : PlayerDeck
    }


type PirateStatus
    = BothPiratesAlive
    | OnePirateDefeated


type OneOrTwo a
    = Two a a
    | One a


type alias HealthLost =
    Int


type ResolvingState
    = PlayerWon HazardCard
    | PlayerLost HealthLost (SelectionList PlayerCard)


type alias AndAnother =
    Bool


type FightView
    = NormalFightView
    | SortView (SortArea PlayerCard)
    | SelectCopyView Int
    | SelectDoubleView Int
    | SelectBelowTheStackView Int
    | SelectExchangeView Int AndAnother
    | SelectDestroyView Int
    | DrawSecondCardView Int


type GameState
    = HazardSelection CommonState (OneOrTwo HazardCard)
    | FightingHazard CommonState FightArea HazardCard FightView
    | ResolvingFight CommonState ResolvingState
    | FinalShowdown CommonState FightArea PirateCard FightView


type GameOverState
    = Win
    | Loss


type Model
    = GameInProgress GameState
    | GameOver GameOverState


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

        -- playerDeck : PlayerDeck
        -- playerDeck =
        --     robinsonCards
        --         |> List.map PlayerCard.fromRobinsonCard
        --         |> PlayerDeck.create agingCards
        playerDeck : PlayerDeck
        playerDeck =
            HazardCard.getTestCards
                |> List.map PlayerCard.fromHazardCard
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
            , pirateStatus = BothPiratesAlive
            , playerDeck = playerDeck
            , hazardDeck = hazardDeck
            }

        hazardSelectionState : OneOrTwo HazardCard
        hazardSelectionState =
            Two leftHazard rightHazard
    in
    -- ( GameOver Win, Cmd.none )
    -- ( GameInProgress (FinalShowdown commonState FightArea.createFightArea pirateOne NormalFightView), Cmd.none )
    ( GameInProgress (HazardSelection commonState hazardSelectionState), Cmd.none )



-- UPDATE


type Msg
    = EndGame
      -- Hazard selection
    | ChooseLeftHazard
    | ChooseRightHazard
    | ChooseSingleHazard
    | ChooseSkipHazard
      -- Fighting hazard/Final Showdown
    | DrawNormally
    | EndFight
    | UseAbility Int
    | CancelAbilitiesInUse
    | SetInUseToUsed
    | SortFinish
    | SortChangeOrder SortArea.ChangeOrderType
    | SortToggleDiscard SortArea.SortIndex
    | SortReveal
    | SelectCopy Int
    | SelectDouble Int
    | SelectBelowTheStack Int
    | SelectExchange Int
    | SelectDestroy Int
    | DrawSecondCard
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

        GameOver _ ->
            ( model, Cmd.none )


{-| Returns the game state to hazard selection with the phase updated, or moves to the final showdown if already in PhaseRed
-}
handlePhaseComplete : CommonState -> GameState
handlePhaseComplete commonState =
    let
        toPhase : Phase -> GameState
        toPhase phase =
            let
                ( shuffledHazardDeck, newSeed ) =
                    Random.step (HazardDeck.reshuffle commonState.hazardDeck) commonState.seed
            in
            case HazardDeck.drawTwice shuffledHazardDeck of
                HazardDeck.NothingDrawn ->
                    -- Should rarely happen that all hazards run out before phase is red, but if it does move directly to final showdown
                    FinalShowdown
                        { commonState | phase = PhaseRed }
                        FightArea.createFightArea
                        commonState.pirateOne
                        NormalFightView

                HazardDeck.DrewOne newHazardDeck hazardCard ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = newSeed } (One hazardCard)

                HazardDeck.DrewTwo newHazardDeck first second ->
                    HazardSelection { commonState | phase = phase, hazardDeck = newHazardDeck, seed = newSeed } (Two first second)
    in
    case commonState.phase of
        PhaseRed ->
            FinalShowdown
                { commonState | phase = PhaseRed }
                FightArea.createFightArea
                commonState.pirateOne
                NormalFightView

        PhaseYellow ->
            toPhase PhaseRed

        PhaseGreen ->
            toPhase PhaseYellow


toFightingHazard : HazardCard -> CommonState -> GameState
toFightingHazard hazard commonState =
    FightingHazard commonState FightArea.createFightArea hazard NormalFightView


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


putOnBottom : PlayerCard -> CommonState -> CommonState
putOnBottom card commonState =
    { commonState | playerDeck = PlayerDeck.putOnBottom card commonState.playerDeck }


discardCard : PlayerCard -> CommonState -> CommonState
discardCard card commonState =
    { commonState | playerDeck = PlayerDeck.discard [ card ] commonState.playerDeck }


discardCardAndDraw : PlayerCard -> CommonState -> ( PlayerCard, CommonState )
discardCardAndDraw card commonState =
    commonState
        |> discardCard card
        |> drawCard
        |> Maybe.withDefault ( card, commonState )


putOnBottomAndDraw : PlayerCard -> CommonState -> ( PlayerCard, CommonState )
putOnBottomAndDraw card commonState =
    commonState
        |> putOnBottom card
        |> drawCard
        |> Maybe.withDefault ( card, commonState )


type DrawCardNormallyResult
    = CantDrawNormally
    | DrawingKillsPlayer
    | DrewCardNormally { newCommonState : CommonState, newFightArea : FightArea }


attemptDrawNormally : CommonState -> FightArea -> Int -> DrawCardNormallyResult
attemptDrawNormally commonState fightArea freeCards =
    case drawCard commonState of
        Nothing ->
            CantDrawNormally

        Just ( drawnCard, onDraw ) ->
            case FightArea.attemptPlayFreeCard drawnCard freeCards fightArea of
                Just newFightArea ->
                    DrewCardNormally { newCommonState = onDraw, newFightArea = newFightArea }

                Nothing ->
                    let
                        newFightArea : FightArea
                        newFightArea =
                            FightArea.playCard drawnCard fightArea

                        maybeNewLifePoints : Maybe LifePoints.Counter
                        maybeNewLifePoints =
                            LifePoints.decrementCounter 1 commonState.lifePoints
                    in
                    case maybeNewLifePoints of
                        Nothing ->
                            DrawingKillsPlayer

                        Just newLifePoints ->
                            let
                                newCommonState : CommonState
                                newCommonState =
                                    { onDraw | lifePoints = newLifePoints }
                            in
                            DrewCardNormally { newCommonState = newCommonState, newFightArea = newFightArea }


type EndFightErr
    = MustDrawACard
    | UnusedAgingCards
    | CantLoseFight


type EndFightOk
    = EndFightPlayerWon
    | EndFightPlayerLost Int


attemptEndFightHazard : Phase -> FightArea -> HazardCard -> Result EndFightErr EndFightOk
attemptEndFightHazard phase fightArea hazard =
    if List.length (FightArea.getCards fightArea) == 0 then
        Err MustDrawACard

    else if FightArea.hasUnusedAgingCards fightArea then
        Err UnusedAgingCards

    else
        let
            playerStrength : Int
            playerStrength =
                FightArea.getPlayerStrength fightArea

            hazardStrength : Int
            hazardStrength =
                if FightArea.isPhaseMinusOne fightArea then
                    case phase of
                        PhaseGreen ->
                            HazardCard.getGreenValue hazard

                        PhaseYellow ->
                            HazardCard.getGreenValue hazard

                        PhaseRed ->
                            HazardCard.getYellowValue hazard

                else
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
        in
        if strengthDifference <= 0 then
            Ok EndFightPlayerWon

        else
            Ok (EndFightPlayerLost strengthDifference)


attemptEndFinalShowdown : FightArea -> PirateCard -> Result EndFightErr EndFightOk
attemptEndFinalShowdown fightArea pirate =
    if FightArea.hasUnusedAgingCards fightArea then
        Err UnusedAgingCards

    else
        let
            playerStrength : Int
            playerStrength =
                FightArea.getPlayerStrength fightArea

            pirateStrength : Int
            pirateStrength =
                PirateCard.getStrength pirate

            strengthDifference : Int
            strengthDifference =
                pirateStrength - playerStrength
        in
        if strengthDifference <= 0 then
            Ok EndFightPlayerWon

        else
            Ok (EndFightPlayerLost strengthDifference)


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
            let
                newHazardDeck : HazardDeck
                newHazardDeck =
                    HazardDeck.discard [ card ] commonState.hazardDeck

                newCommonState : CommonState
                newCommonState =
                    { commonState | hazardDeck = newHazardDeck }
            in
            ( GameInProgress (handlePhaseComplete newCommonState), Cmd.none )

        -- Fight
        ( DrawNormally, FightingHazard commonState fightArea hazardCard NormalFightView ) ->
            let
                freeDraws : Int
                freeDraws =
                    HazardCard.getFreeCards hazardCard
            in
            case attemptDrawNormally commonState fightArea freeDraws of
                DrawingKillsPlayer ->
                    ( GameOver Loss, Cmd.none )

                CantDrawNormally ->
                    noOp

                DrewCardNormally { newCommonState, newFightArea } ->
                    ( GameInProgress (FightingHazard newCommonState newFightArea hazardCard NormalFightView), Cmd.none )

        ( DrawNormally, FinalShowdown commonState fightArea pirateCard NormalFightView ) ->
            let
                freeDraws : Int
                freeDraws =
                    PirateCard.getFreeCards pirateCard
            in
            case attemptDrawNormally commonState fightArea freeDraws of
                DrawingKillsPlayer ->
                    ( GameOver Loss, Cmd.none )

                CantDrawNormally ->
                    noOp

                DrewCardNormally { newCommonState, newFightArea } ->
                    ( GameInProgress (FinalShowdown newCommonState newFightArea pirateCard NormalFightView), Cmd.none )

        ( EndFight, FightingHazard commonState fightArea hazard NormalFightView ) ->
            case attemptEndFightHazard commonState.phase fightArea hazard of
                Err _ ->
                    noOp

                Ok EndFightPlayerWon ->
                    let
                        discardedCards : List PlayerCard
                        discardedCards =
                            FightArea.getCards fightArea

                        newPlayerDeck : PlayerDeck
                        newPlayerDeck =
                            PlayerDeck.discard discardedCards commonState.playerDeck

                        newCommonState : CommonState
                        newCommonState =
                            { commonState | playerDeck = newPlayerDeck }

                        resolvingState : ResolvingState
                        resolvingState =
                            PlayerWon hazard
                    in
                    ( GameInProgress (ResolvingFight newCommonState resolvingState), Cmd.none )

                Ok (EndFightPlayerLost strengthDifference) ->
                    case LifePoints.decrementCounter strengthDifference commonState.lifePoints of
                        Nothing ->
                            ( GameOver Loss, Cmd.none )

                        Just newLifePoints ->
                            let
                                newHazardDeck : HazardDeck
                                newHazardDeck =
                                    HazardDeck.discard [ hazard ] commonState.hazardDeck

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

        ( EndFight, FinalShowdown commonState fightArea pirate NormalFightView ) ->
            case attemptEndFinalShowdown fightArea pirate of
                Err _ ->
                    noOp

                Ok (EndFightPlayerLost _) ->
                    -- shouldn't happen
                    noOp

                Ok EndFightPlayerWon ->
                    case commonState.pirateStatus of
                        BothPiratesAlive ->
                            let
                                discardedCards : List PlayerCard
                                discardedCards =
                                    FightArea.getCards fightArea

                                newPlayerDeck : PlayerDeck
                                newPlayerDeck =
                                    PlayerDeck.discard discardedCards commonState.playerDeck

                                newCommonState : CommonState
                                newCommonState =
                                    { commonState | playerDeck = newPlayerDeck, pirateStatus = OnePirateDefeated }

                                newGameState : GameState
                                newGameState =
                                    FinalShowdown newCommonState FightArea.createFightArea commonState.pirateTwo NormalFightView
                            in
                            ( GameInProgress newGameState, Cmd.none )

                        OnePirateDefeated ->
                            ( GameOver Win, Cmd.none )

        ( UseAbility index, FightingHazard commonState fightArea hazard NormalFightView ) ->
            FightArea.attemptUse index fightArea
                |> Result.andThen
                    (\( ability, { setCardInUse, setCardUsed } ) ->
                        attemptResolveAbility
                            { ability = ability
                            , index = index
                            , setCardInUse = setCardInUse
                            , setCardUsed = setCardUsed
                            , commonState = commonState
                            , fightArea = fightArea
                            }
                    )
                |> Result.map
                    (\( newCommonState, newFightArea, newFightView ) ->
                        ( GameInProgress (FightingHazard newCommonState newFightArea hazard newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( UseAbility index, FinalShowdown commonState fightArea hazard NormalFightView ) ->
            FightArea.attemptUse index fightArea
                |> Result.andThen
                    (\( ability, { setCardInUse, setCardUsed } ) ->
                        attemptResolveAbility
                            { ability = ability
                            , index = index
                            , setCardInUse = setCardInUse
                            , setCardUsed = setCardUsed
                            , commonState = commonState
                            , fightArea = fightArea
                            }
                    )
                |> Result.map
                    (\( newCommonState, newFightArea, newFightView ) ->
                        ( GameInProgress (FinalShowdown newCommonState newFightArea hazard newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( CancelAbilitiesInUse, FightingHazard commonState fightArea hazard _ ) ->
            ( GameInProgress (FightingHazard commonState (FightArea.undoAllInUse fightArea) hazard NormalFightView), Cmd.none )

        ( CancelAbilitiesInUse, FinalShowdown commonState fightArea pirate _ ) ->
            ( GameInProgress (FinalShowdown commonState (FightArea.undoAllInUse fightArea) pirate NormalFightView), Cmd.none )

        ( SetInUseToUsed, FightingHazard commonState fightArea hazard _ ) ->
            ( GameInProgress (FightingHazard commonState (FightArea.setInUseToUsed fightArea) hazard NormalFightView), Cmd.none )

        ( SetInUseToUsed, FinalShowdown commonState fightArea hazard _ ) ->
            ( GameInProgress (FinalShowdown commonState (FightArea.setInUseToUsed fightArea) hazard NormalFightView), Cmd.none )

        ( SortFinish, FightingHazard commonState fightArea hazard (SortView sortArea) ) ->
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

                newFightArea : FightArea
                newFightArea =
                    FightArea.setInUseToUsed fightArea
            in
            ( GameInProgress (FightingHazard newCommonState newFightArea hazard NormalFightView), Cmd.none )

        ( SortFinish, FinalShowdown commonState fightArea pirate (SortView sortArea) ) ->
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

                newFightArea : FightArea
                newFightArea =
                    FightArea.setInUseToUsed fightArea
            in
            ( GameInProgress (FinalShowdown newCommonState newFightArea pirate NormalFightView), Cmd.none )

        ( SortChangeOrder changeOrderType, FightingHazard commonState fightArea hazard (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.changeOrder changeOrderType sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea hazard (SortView newSortArea)), Cmd.none )

        ( SortChangeOrder changeOrderType, FinalShowdown commonState fightArea pirate (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.changeOrder changeOrderType sortArea
            in
            ( GameInProgress (FinalShowdown commonState fightArea pirate (SortView newSortArea)), Cmd.none )

        ( SortToggleDiscard sortIndex, FightingHazard commonState fightArea hazard (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.toggleDiscard sortIndex sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea hazard (SortView newSortArea)), Cmd.none )

        ( SortToggleDiscard sortIndex, FinalShowdown commonState fightArea pirate (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.toggleDiscard sortIndex sortArea
            in
            ( GameInProgress (FinalShowdown commonState fightArea pirate (SortView newSortArea)), Cmd.none )

        ( SortReveal, FightingHazard commonState fightArea hazard (SortView sortArea) ) ->
            case ( SortArea.attemptReveal sortArea, drawCard commonState ) of
                ( Just onReveal, Just ( drawnCard, newCommonState ) ) ->
                    let
                        newSortArea : SortArea PlayerCard
                        newSortArea =
                            onReveal drawnCard
                    in
                    ( GameInProgress (FightingHazard newCommonState fightArea hazard (SortView newSortArea)), Cmd.none )

                _ ->
                    noOp

        ( SortReveal, FinalShowdown commonState fightArea pirate (SortView sortArea) ) ->
            case ( SortArea.attemptReveal sortArea, drawCard commonState ) of
                ( Just onReveal, Just ( drawnCard, newCommonState ) ) ->
                    let
                        newSortArea : SortArea PlayerCard
                        newSortArea =
                            onReveal drawnCard
                    in
                    ( GameInProgress (FinalShowdown newCommonState fightArea pirate (SortView newSortArea)), Cmd.none )

                _ ->
                    noOp

        ( SelectCopy index, FightingHazard commonState fightArea hazard (SelectCopyView copyIndex) ) ->
            if index == copyIndex then
                noOp

            else
                FightArea.attemptUse index fightArea
                    |> Result.andThen
                        (\( ability, _ ) ->
                            attemptResolveAbility
                                { ability = ability
                                , index = index
                                , setCardInUse = fightArea
                                , setCardUsed = fightArea
                                , commonState = commonState
                                , fightArea = fightArea
                                }
                        )
                    |> Result.map
                        (\( newCommonState, newFightArea, newFightView ) ->
                            ( GameInProgress (FightingHazard newCommonState newFightArea hazard newFightView), Cmd.none )
                        )
                    |> Result.withDefault noOp

        ( SelectCopy index, FinalShowdown commonState fightArea pirate (SelectCopyView copyIndex) ) ->
            if index == copyIndex then
                noOp

            else
                FightArea.attemptUse index fightArea
                    |> Result.andThen
                        (\( ability, _ ) ->
                            attemptResolveAbility
                                { ability = ability
                                , index = index
                                , setCardInUse = fightArea
                                , setCardUsed = fightArea
                                , commonState = commonState
                                , fightArea = fightArea
                                }
                        )
                    |> Result.map
                        (\( newCommonState, newFightArea, newFightView ) ->
                            ( GameInProgress (FinalShowdown newCommonState newFightArea pirate newFightView), Cmd.none )
                        )
                    |> Result.withDefault noOp

        ( SelectDouble index, FightingHazard commonState fightArea hazard (SelectDoubleView doubleIndex) ) ->
            if index == doubleIndex then
                noOp

            else
                case FightArea.attemptDouble index fightArea of
                    Just newFightArea ->
                        ( GameInProgress (FightingHazard commonState (FightArea.setInUseToUsed newFightArea) hazard NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectDouble index, FinalShowdown commonState fightArea pirate (SelectDoubleView doubleIndex) ) ->
            if index == doubleIndex then
                noOp

            else
                case FightArea.attemptDouble index fightArea of
                    Just newFightArea ->
                        ( GameInProgress (FinalShowdown commonState (FightArea.setInUseToUsed newFightArea) pirate NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectBelowTheStack index, FightingHazard commonState fightArea hazard (SelectBelowTheStackView belowTheStackIndex) ) ->
            if index == belowTheStackIndex then
                -- card cannot use ability on itself
                noOp

            else
                case FightArea.attemptExchange index fightArea of
                    Just ( playerCard, onExchange ) ->
                        let
                            ( drawnCard, newCommonState ) =
                                putOnBottomAndDraw playerCard commonState
                        in
                        ( GameInProgress (FightingHazard newCommonState (onExchange drawnCard) hazard NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectBelowTheStack index, FinalShowdown commonState fightArea pirate (SelectBelowTheStackView belowTheStackIndex) ) ->
            if index == belowTheStackIndex then
                -- card cannot use ability on itself
                noOp

            else
                case FightArea.attemptExchange index fightArea of
                    Just ( playerCard, onExchange ) ->
                        let
                            ( drawnCard, newCommonState ) =
                                putOnBottomAndDraw playerCard commonState
                        in
                        ( GameInProgress (FinalShowdown newCommonState (onExchange drawnCard) pirate NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectExchange index, FightingHazard commonState fightArea hazard (SelectExchangeView exchangeIndex andAnother) ) ->
            if index == exchangeIndex then
                noOp

            else
                case FightArea.attemptExchange index fightArea of
                    Just ( playerCard, onExchange ) ->
                        let
                            ( drawnCard, newCommonState ) =
                                discardCardAndDraw playerCard commonState

                            newFightView : FightView
                            newFightView =
                                if andAnother then
                                    SelectExchangeView exchangeIndex False

                                else
                                    NormalFightView

                            newFightArea : FightArea
                            newFightArea =
                                if andAnother then
                                    onExchange drawnCard

                                else
                                    FightArea.setInUseToUsed (onExchange drawnCard)
                        in
                        ( GameInProgress (FightingHazard newCommonState newFightArea hazard newFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectExchange index, FinalShowdown commonState fightArea pirate (SelectExchangeView exchangeIndex andAnother) ) ->
            if index == exchangeIndex then
                noOp

            else
                case FightArea.attemptExchange index fightArea of
                    Just ( playerCard, onExchange ) ->
                        let
                            ( drawnCard, newCommonState ) =
                                discardCardAndDraw playerCard commonState

                            newFightView : FightView
                            newFightView =
                                if andAnother then
                                    SelectExchangeView exchangeIndex False

                                else
                                    NormalFightView

                            newFightArea : FightArea
                            newFightArea =
                                if andAnother then
                                    onExchange drawnCard

                                else
                                    FightArea.setInUseToUsed (onExchange drawnCard)
                        in
                        ( GameInProgress (FinalShowdown newCommonState newFightArea pirate newFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectDestroy index, FightingHazard commonState fightArea hazard (SelectDestroyView destroyIndex) ) ->
            if index == destroyIndex then
                noOp

            else
                case FightArea.attemptDestroy index fightArea of
                    Just onDestroy ->
                        let
                            newFightArea : FightArea
                            newFightArea =
                                FightArea.setInUseToUsed onDestroy
                        in
                        ( GameInProgress (FightingHazard commonState newFightArea hazard NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectDestroy index, FinalShowdown commonState fightArea pirate (SelectDestroyView destroyIndex) ) ->
            if index == destroyIndex then
                noOp

            else
                case FightArea.attemptDestroy index fightArea of
                    Just onDestroy ->
                        let
                            newFightArea : FightArea
                            newFightArea =
                                FightArea.setInUseToUsed onDestroy
                        in
                        ( GameInProgress (FinalShowdown commonState newFightArea pirate NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        -- TODO: DrawSecondCard in FinalShowdown
        ( DrawSecondCard, FightingHazard commonState fightArea hazard (DrawSecondCardView drawTwoIndex) ) ->
            case drawCard commonState of
                Nothing ->
                    noOp

                Just ( drawnCard, newCommonState ) ->
                    let
                        newFightArea : FightArea
                        newFightArea =
                            fightArea
                                |> FightArea.playCard drawnCard
                                |> FightArea.setInUseToUsed
                    in
                    ( GameInProgress (FightingHazard newCommonState newFightArea hazard NormalFightView), Cmd.none )

        ( AcceptWin, ResolvingFight commonState (PlayerWon hazardCard) ) ->
            let
                newCommonState : CommonState
                newCommonState =
                    { commonState | playerDeck = PlayerDeck.discard [ PlayerCard.fromHazardCard hazardCard ] commonState.playerDeck }
            in
            case HazardDeck.drawTwice commonState.hazardDeck of
                HazardDeck.NothingDrawn ->
                    ( GameInProgress (handlePhaseComplete newCommonState), Cmd.none )

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
                    ( GameInProgress (handlePhaseComplete newCommonState), Cmd.none )

                HazardDeck.DrewOne newHazardDeck card ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (One card)), Cmd.none )

                HazardDeck.DrewTwo newHazardDeck first second ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (Two first second)), Cmd.none )

        _ ->
            noOp


type alias ResolveAbilityArg =
    { ability : SpecialAbility
    , index : Int
    , setCardInUse : FightArea
    , setCardUsed : FightArea
    , commonState : CommonState
    , fightArea : FightArea
    }


attemptResolveAbility : ResolveAbilityArg -> Result String ( CommonState, FightArea, FightView )
attemptResolveAbility { ability, index, setCardInUse, setCardUsed, commonState, fightArea } =
    case ability of
        PlusOneLife ->
            if LifePoints.isFull commonState.lifePoints then
                Err "Life counter is full"

            else
                let
                    newLifePoints : LifePoints.Counter
                    newLifePoints =
                        LifePoints.incrementCounter commonState.lifePoints

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | lifePoints = newLifePoints }

                    newFightArea : FightArea
                    newFightArea =
                        setCardUsed
                in
                Ok ( newCommonState, newFightArea, NormalFightView )

        PlusTwoLife ->
            if LifePoints.isFull commonState.lifePoints then
                Err "Life counter is full"

            else
                let
                    newLifePoints : LifePoints.Counter
                    newLifePoints =
                        commonState.lifePoints
                            |> LifePoints.incrementCounter
                            |> LifePoints.incrementCounter

                    newCommonState : CommonState
                    newCommonState =
                        { commonState | lifePoints = newLifePoints }

                    newFightArea : FightArea
                    newFightArea =
                        setCardUsed
                in
                Ok ( newCommonState, newFightArea, NormalFightView )

        SortThree ->
            case drawCard commonState of
                Nothing ->
                    Err "Unable to draw card"

                Just ( drawnCard, newCommonState ) ->
                    Ok ( newCommonState, setCardInUse, SortView (SortArea.create drawnCard) )

        -- TODO: return Err for all of these card-targeting abilities if no other eligible cards
        Copy ->
            if List.length (FightArea.getAbilityCards fightArea) <= 1 then
                Err "No card available to copy"

            else
                Ok ( commonState, setCardInUse, SelectCopyView index )

        Double ->
            let
                cardAvailableToDouble : Bool
                cardAvailableToDouble =
                    case FightArea.undoubledCardIndexes fightArea of
                        [] ->
                            False

                        [ singleIndex ] ->
                            if singleIndex == index then
                                -- Only card available to double is itself which isn't allowed
                                False

                            else
                                True

                        _ ->
                            True
            in
            if cardAvailableToDouble then
                Ok ( commonState, setCardInUse, SelectDoubleView index )

            else
                Err "No card available to double"

        BelowTheStack ->
            if List.length (FightArea.getCards fightArea) <= 1 then
                Err "No card available to put below the stack"

            else
                Ok ( commonState, setCardInUse, SelectBelowTheStackView index )

        ExchangeTwo ->
            if List.length (FightArea.getCards fightArea) <= 2 then
                Err "No card available to exchange"

            else
                Ok ( commonState, setCardInUse, SelectExchangeView index True )

        Destroy ->
            if List.length (FightArea.getCards fightArea) <= 1 then
                Err "No card available to destroy"

            else
                Ok ( commonState, setCardInUse, SelectDestroyView index )

        PhaseMinusOne ->
            if commonState.phase == PhaseGreen then
                Err "Phase cannot go below Green"

            else
                Ok ( commonState, FightArea.setPhaseMinusOne setCardUsed, NormalFightView )

        DrawOne ->
            case drawCard commonState of
                Nothing ->
                    Err "Unable to draw card"

                Just ( drawnCard, newCommonState ) ->
                    Ok ( newCommonState, FightArea.playCard drawnCard setCardUsed, NormalFightView )

        DrawTwo ->
            case drawCard commonState of
                Nothing ->
                    Err "Unable to draw card"

                Just ( drawnCard, newCommonState ) ->
                    case drawCard newCommonState of
                        Nothing ->
                            Ok ( newCommonState, FightArea.playCard drawnCard setCardUsed, NormalFightView )

                        Just _ ->
                            Ok ( newCommonState, FightArea.playCard drawnCard setCardInUse, DrawSecondCardView (index + 1) )

        {--|
            ExchangeOne
            MinusOneLife
            MinusTwoLife
            HighestCardNull
            StopDrawing
        --}
        _ ->
            Debug.todo "Implement missing ability"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Common


renderPhase : Phase -> Html Msg
renderPhase phase =
    case phase of
        PhaseGreen ->
            ul []
                [ li [ class "text-green-600 font-bold text-lg" ] [ text "Green  ← " ]
                , li [ class "text-yellow-500 font-thin text-lg" ] [ text "Yellow" ]
                , li [ class "text-red-600 text-lg" ] [ text "Red" ]
                ]

        PhaseYellow ->
            ul []
                [ li [ class "text-green-600 text-lg" ] [ text "Green" ]
                , li [ class "text-yellow-500 font-bold text-lg" ] [ text "Yellow  ← " ]
                , li [ class "text-red-600 text-lg" ] [ text "Red" ]
                ]

        PhaseRed ->
            ul []
                [ li [ class "text-green-600 font-thin text-lg" ] [ text "Green" ]
                , li [ class "text-yellow-500 font-thin text-lg" ] [ text "Yellow" ]
                , li [ class "text-red-600 font-bold text-lg" ] [ text "Red  ← " ]
                ]


renderCommonState : CommonState -> Html Msg
renderCommonState commonState =
    let
        renderDrawPile : Int -> Html Msg
        renderDrawPile count =
            div [ class "flex items-end space-x-2" ]
                [ div [ class "border-4 border-blue-300 rounded w-10 h-16 bg-blue-600 mt-1 p-1 shadow" ] []
                , div [] [ text ("x " ++ String.fromInt count) ]
                ]

        renderDiscardPile : Int -> Html Msg
        renderDiscardPile count =
            div [ class "flex items-end space-x-2" ]
                [ div [ class "border border-blue-300 border-dashed rounded w-10 h-16 mt-1" ] []
                , div [] [ text ("x " ++ String.fromInt count) ]
                ]

        renderHazardPile : Int -> Html Msg
        renderHazardPile count =
            div [ class "flex items-end space-x-2" ]
                [ div [ class "border-4 border-orange-300 rounded w-10 h-16 bg-orange-600 mt-1 p-1 shadow" ] []
                , div [] [ text ("x " ++ String.fromInt count) ]
                ]

        renderHazardDiscard : Int -> Html Msg
        renderHazardDiscard count =
            div [ class "flex items-end space-x-2" ]
                [ div [ class "border border-orange-300 border-dashed rounded w-10 h-16 mt-1" ] []
                , div [] [ text ("x " ++ String.fromInt count) ]
                ]
    in
    div [ class leftColClasses ]
        [ div [ class "flex flex-col h-32 items-center justify-center border-b border-blue-100" ]
            [ img [ class "w-24 h-24 rounded-full", src "robinson-pic.jpg" ] []
            , div [ class "text-xl font-bold" ] [ text "Robinson" ]
            ]
        , div [ class "flex flex-col" ]
            [ span [ class "text-sm" ] [ text "Health" ]
            , span []
                [ span [ class "text-4xl font-bold leading-8 tracking-wide" ] [ text (String.fromInt (LifePoints.getValue commonState.lifePoints)) ]
                , span [] [ text " / 22" ]
                ]
            ]
        , div [ class "flex flex-col" ]
            [ span [ class "text-sm" ] [ text "Robinson Deck" ]
            , renderDrawPile (PlayerDeck.drawPileCount commonState.playerDeck)
            , span [ class "text-sm mt-2" ] [ text "Robinson Discard" ]
            , renderDiscardPile (PlayerDeck.discardPileCount commonState.playerDeck)
            ]
        , div [ class "flex flex-col" ]
            [ span [ class "text-sm" ] [ text "Hazard Deck" ]
            , renderHazardPile (HazardDeck.drawPileCount commonState.hazardDeck)
            , span [ class "text-sm mt-2" ] [ text "Hazard Discard" ]
            , renderHazardDiscard (HazardDeck.discardPileCount commonState.hazardDeck)
            ]
        , div [ class "flex flex-col" ]
            [ span [ class "text-sm" ] [ text "Phase" ]
            , renderPhase commonState.phase
            ]
        ]


statCircle : String
statCircle =
    "rounded-full h-8 w-8 flex items-center justify-center font-semibold text-sm"


statTransparent : String
statTransparent =
    statCircle ++ " border border-gray-100"


statWhite : String
statWhite =
    statCircle ++ " bg-white text-gray-900 border border-gray-900"


statGreen : String
statGreen =
    statCircle ++ " bg-green-600 text-gray-100 border border-gray-10"


statYellow : String
statYellow =
    statCircle ++ " bg-yellow-400 text-gray-900 border border-gray-900"


statRed : String
statRed =
    statCircle ++ " bg-red-600 text-gray-100 border border-gray-100"


renderHazard : Phase -> HazardCard -> Html Msg
renderHazard phase hazardCard =
    div [ class "flex flex-col bg-orange-300 h-32 w-64 p-2 border-8 border-orange-600 rounded border-white text-orange-800 relative" ]
        [ div [ class "font-bold mb-2" ] [ text (HazardCard.getTitle hazardCard) ]
        , div [ class "flex items-center mb-1" ]
            [ div [ class "text-sm mr-2" ] [ text "Strength: " ]
            , case phase of
                PhaseGreen ->
                    div [ class statGreen ]
                        [ text <| String.fromInt <| HazardCard.getGreenValue hazardCard
                        ]

                PhaseYellow ->
                    div [ class statYellow ]
                        [ text <| String.fromInt <| HazardCard.getYellowValue hazardCard
                        ]

                PhaseRed ->
                    div [ class statRed ]
                        [ text <| String.fromInt <| HazardCard.getRedValue hazardCard
                        ]
            ]
        , div [ class "flex items-center" ]
            [ div [ class "text-sm mr-2" ] [ text "Free cards: " ]
            , div [ class statWhite ]
                [ text <| String.fromInt <| HazardCard.getFreeCards hazardCard
                ]
            ]

        -- TODO: implement view reward after renderFightingCard is implemented
        -- TODO: Switch this text out for an icon button
        , button [ class "absolute bottom-0 right-0" ] [ text "View Reward" ]
        ]



-- HazardSelection


renderHazardChoice : Phase -> OneOrTwo HazardCard -> Html Msg
renderHazardChoice phase hazardOption =
    let
        renderChooseButton : String -> Msg -> Html Msg
        renderChooseButton buttonLabel handleClick =
            button [ class "bg-gray-100 hover:bg-gray-300 font-bold text-gray-800 rounded py-2 px-4 rounded", onClick handleClick ]
                [ text buttonLabel
                ]
    in
    div [ class rightColClasses ]
        [ div [ class "flex flex-col h-32 items-center justify-center" ]
            [ div [ class "text-3xl font-bold" ] [ text "Choose Hazard" ]
            ]
        , case hazardOption of
            Two leftHazard rightHazard ->
                div [ class "flex justify-center space-x-24" ]
                    [ div [ class "flex flex-col items-center space-y-4" ]
                        [ renderHazard phase leftHazard
                        , renderChooseButton "Choose" ChooseLeftHazard
                        ]
                    , div [ class "flex flex-col items-center space-y-4" ]
                        [ renderHazard phase rightHazard
                        , renderChooseButton "Choose" ChooseRightHazard
                        ]
                    ]

            One hazard ->
                div [ class "flex justify-center space-x-24" ]
                    [ div [ class "flex flex-col items-center space-y-4" ]
                        [ renderHazard phase hazard
                        , renderChooseButton "Choose" ChooseSingleHazard
                        ]
                    , div [ class "flex flex-col items-center space-y-4" ]
                        [ div [ class "invisible" ] [ renderHazard phase hazard ]
                        , renderChooseButton "Skip" ChooseSkipHazard
                        ]
                    ]
        ]


renderHazardSelection : CommonState -> OneOrTwo HazardCard -> Html Msg
renderHazardSelection commonState hazardOption =
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , renderHazardChoice commonState.phase hazardOption
        ]



-- FightingHazard


getHazardStrength : Phase -> HazardCard -> Int
getHazardStrength phase hazard =
    case phase of
        PhaseGreen ->
            HazardCard.getGreenValue hazard

        PhaseYellow ->
            HazardCard.getYellowValue hazard

        PhaseRed ->
            HazardCard.getRedValue hazard


type alias FightDashArgs =
    { canDraw : Bool
    , fightArea : FightArea
    , renderEnemy : Html Msg
    , freeCards : Int
    , enemyStrength : Int
    , endFightResult : Result EndFightErr EndFightOk
    }


renderFightDash : FightDashArgs -> Html Msg
renderFightDash { canDraw, fightArea, renderEnemy, freeCards, enemyStrength, endFightResult } =
    div [ class "flex items-center justify-between" ]
        [ div [ class "flex flex-col items-start space-y-4 px-8" ]
            [ div [ class "flex" ]
                [ div [ class "flex items-end" ]
                    [ if canDraw then
                        -- TODO: disable if not in NormalFightView
                        button [ class standardButton, onClick DrawNormally ] [ text "Draw" ]

                      else
                        button [ class disabledStandardButton, disabled True ] [ text "Draw" ]
                    , span [ class "text-3xl font-bold mr-1 leading-none" ]
                        [ text <| String.fromInt (FightArea.getFreeCardsDrawn fightArea) ]
                    , span [ class "text-3xl mr-1 leading-none" ] [ text "/" ]
                    , span [ class "text-3xl font-bold mr-2 leading-none" ] [ text <| String.fromInt freeCards ]
                    , span [ class "leading-none" ] [ text "free cards drawn" ]
                    ]
                ]
            , div [ class "flex" ]
                [ div [ class "flex items-end" ]
                    [ case endFightResult of
                        -- TODO: enhance this button with affordances for why fight can't be ended based on error type
                        Err _ ->
                            button [ class disabledStandardButton, disabled True ] [ text "End Fight" ]

                        -- TODO: enhance this button with affordances to let player know if they'll win or lose
                        Ok _ ->
                            button [ class standardButton, onClick EndFight ] [ text "End Fight" ]
                    , span [ class "text-3xl font-bold mr-1 leading-none" ]
                        [ text <| String.fromInt (FightArea.getPlayerStrength fightArea) ]
                    , span [ class "text-3xl mr-1 leading-none" ] [ text "/" ]
                    , span [ class "text-3xl font-bold mr-2 leading-none" ] [ text <| String.fromInt enemyStrength ]
                    , span [ class "leading-none" ] [ text "total strength" ]
                    ]
                ]
            ]
        , div []
            [ renderEnemy ]

        -- Below is an invisible copy of earlier button cluster for purposes of centering the hazard card using justify-between
        , div [ class "invisible flex flex-col items-start space-y-4 px-8" ]
            [ div [ class "flex" ]
                [ div [ class "flex items-end" ]
                    [ button [ class "mr-4 inline-block bg-gray-100 hover:bg-gray-300 font-bold text-gray-800 rounded py-2 px-4 rounded", onClick DrawNormally ] [ text "Draw" ]
                    , span [ class "text-3xl font-bold mr-1 leading-none" ]
                        [ text <| String.fromInt (FightArea.getFreeCardsDrawn fightArea) ]
                    , span [ class "text-3xl mr-1 leading-none" ] [ text "/" ]
                    , span [ class "text-3xl font-bold mr-2 leading-none" ] [ text <| String.fromInt freeCards ]
                    , span [ class "leading-none" ] [ text "free cards drawn" ]
                    ]
                ]
            , button [ class "inline-block bg-gray-100 hover:bg-gray-300 font-bold text-gray-800 rounded py-2 px-4 rounded", onClick EndFight ] [ text "End Fight" ]
            ]
        ]


renderPlayerCard : PlayerCard -> Bool -> Html Msg
renderPlayerCard playerCard isDoubled =
    div [ class "flex flex-col bg-blue-300 h-32 w-64 p-2 border-8 border-blue-600 rounded border-white text-blue-800 relative" ]
        [ div [ class "font-bold mb-2" ] [ text (PlayerCard.getTitle playerCard) ]
        , div [ class "flex items-center mb-1" ]
            [ div [ class "text-sm mr-2" ] [ text "Strength: " ]
            , if isDoubled then
                div [ class "flex items-center" ]
                    [ div [ class <| statWhite ++ "font-bold underline text-green-600" ]
                        [ text <| String.fromInt <| PlayerCard.getStrength playerCard * 2
                        ]
                    , div [ class "ml-2" ] [ text "*Doubled" ]
                    ]

              else
                div [ class statWhite ]
                    [ text <| String.fromInt <| PlayerCard.getStrength playerCard
                    ]
            ]
        , case PlayerCard.getAbility playerCard of
            Just ability ->
                div [ class "flex items-center mb-1" ]
                    [ div [ class "text-sm mr-2" ] [ text "Ability: " ]
                    , div [ class "text-sm" ] [ text <| FightStats.getAbilityLabel ability ]
                    ]

            Nothing ->
                div [] []
        ]


standardButton : String
standardButton =
    "mr-4 inline-block bg-gray-100 hover:bg-gray-300 font-bold text-gray-800 rounded py-2 px-4 rounded"


disabledStandardButton : String
disabledStandardButton =
    "mr-4 inline-block bg-gray-100 opacity-50 font-bold text-gray-800 rounded py-2 px-4 rounded"


transparentButton : String
transparentButton =
    "inline-block border border-gray-100 hover:bg-gray-100 hover:bg-opacity-25 rounded px-2 py-1 text-gray-100"


disabledTransparentButton : String
disabledTransparentButton =
    "inline-block border border-gray-500 rounded px-2 py-1 text-gray-500"


renderPlayedCard : CommonState -> FightArea -> FightView -> Int -> FightArea.PlayedCard -> Html Msg
renderPlayedCard commonState fightArea fightView index playedCard =
    case playedCard of
        FightArea.NormalCard card isDoubled ->
            div [ class "flex flex-col items-center" ]
                [ div [ class "mr-4 mb-4" ] [ renderPlayerCard card isDoubled ]
                , if isDoubled then
                    div [] [ text "Doubled" ]

                  else
                    div [] []
                ]

        FightArea.AbilityCard card usedState isDoubled ->
            div [ class "flex flex-col items-center" ]
                [ div [ class "mr-4 mb-4" ] [ renderPlayerCard card isDoubled ]
                , case fightView of
                    NormalFightView ->
                        case ( PlayerCard.getAbility card, usedState ) of
                            ( Just ability, FightArea.NotUsed ) ->
                                let
                                    arg : ResolveAbilityArg
                                    arg =
                                        { ability = ability
                                        , index = index
                                        , setCardInUse = fightArea
                                        , setCardUsed = fightArea
                                        , commonState = commonState
                                        , fightArea = fightArea
                                        }

                                    isAbilityDisabled : Bool
                                    isAbilityDisabled =
                                        Result.Extra.isErr <|
                                            attemptResolveAbility arg
                                in
                                button
                                    [ class <|
                                        if isAbilityDisabled then
                                            disabledTransparentButton

                                        else
                                            transparentButton
                                    , onClick <| UseAbility index
                                    , disabled isAbilityDisabled
                                    ]
                                    [ text <| FightStats.getAbilityLabel ability ]

                            ( Just _, FightArea.InUse ) ->
                                div [ class "px-2 py-1" ] [ text "In Use" ]

                            ( Just ability, FightArea.Used ) ->
                                button [ class disabledTransparentButton, disabled True ] [ text <| FightStats.getAbilityLabel ability ]

                            ( Nothing, _ ) ->
                                div [] []

                    SelectDoubleView doubleIndex ->
                        if isDoubled then
                            -- can't double the same card twice
                            div [] []

                        else if index == doubleIndex then
                            button [ class "border border-red-500 hover:bg-red-500 hover:bg-opacity-25 hover:text-gray-100 rounded px-2 py-1 text-red-500", onClick <| CancelAbilitiesInUse ] [ text "Cancel" ]

                        else
                            button [ class transparentButton, onClick <| SelectDouble index ] [ text "Select Double" ]

                    SelectDestroyView destroyIndex ->
                        if index == destroyIndex then
                            button [ class "border border-red-500 hover:bg-red-500 hover:bg-opacity-25 hover:text-gray-100 rounded px-2 py-1 text-red-500", onClick <| CancelAbilitiesInUse ] [ text "Cancel" ]

                        else
                            button [ class transparentButton, onClick <| SelectDestroy index ] [ text "Select Destroy" ]

                    DrawSecondCardView drawTwoIndex ->
                        if index == drawTwoIndex then
                            div [ class "flex flex-col justify-center" ]
                                [ button [ class transparentButton, onClick DrawSecondCard ] [ text "Draw Again" ]
                                , button [ class transparentButton, onClick SetInUseToUsed ] [ text "Stop" ]
                                ]

                        else
                            div [] []

                    _ ->
                        Debug.todo "Implement missing fight view"
                ]


renderSortAreaCard : SortArea.SortIndex -> PlayerCard -> Bool -> Html Msg
renderSortAreaCard sortIndex playerCard isDiscarded =
    div [ class "flex flex-col items-center" ]
        [ div [ class "mr-4 mb-4" ] [ renderPlayerCard playerCard False ]
        , if isDiscarded then
            div [] [ text "Marked for discard" ]

          else
            div [] []

        -- TODO: support change sort order
        , div [ class "flex items-center" ]
            [ button [] [ text "<" ]
            , button [ class transparentButton, onClick (SortToggleDiscard sortIndex) ] [ text "Toggle discard" ]
            ]
        ]


type SortMoveButtonSet
    = SortMoveButtonsNone
    | SortMoveButtonsLeft SortArea.ChangeOrderType
    | SortMoveButtonsRight SortArea.ChangeOrderType
    | SortMoveButtonsBoth


renderSortArea : SortArea PlayerCard -> Html Msg
renderSortArea sortArea =
    let
        sortArrow : String
        sortArrow =
            statTransparent ++ " mx-2 focus:outline-none"

        renderLeftArrow : SortMoveButtonSet -> Html Msg
        renderLeftArrow sortMoveButtonSet =
            case sortMoveButtonSet of
                SortMoveButtonsNone ->
                    button [ class sortArrow, class "opacity-0" ] [ text "<" ]

                SortMoveButtonsRight changeOrderType ->
                    button [ class sortArrow, class "opacity-0" ] [ text "<" ]

                SortMoveButtonsLeft changeOrderType ->
                    button [ class sortArrow, onClick <| SortChangeOrder changeOrderType ] [ text "<" ]

                SortMoveButtonsBoth ->
                    button [ class sortArrow, onClick <| SortChangeOrder SortArea.OneToTwo ] [ text "<" ]

        renderRightArrow : SortMoveButtonSet -> Html Msg
        renderRightArrow sortMoveButtonSet =
            case sortMoveButtonSet of
                SortMoveButtonsNone ->
                    button [ class sortArrow, class "opacity-0" ] [ text ">" ]

                SortMoveButtonsLeft changeOrderType ->
                    button [ class sortArrow, class "opacity-0" ] [ text ">" ]

                SortMoveButtonsRight changeOrderType ->
                    button [ class sortArrow, onClick <| SortChangeOrder changeOrderType ] [ text ">" ]

                SortMoveButtonsBoth ->
                    button [ class sortArrow, onClick <| SortChangeOrder SortArea.TwoToThree ] [ text ">" ]

        renderSortCard : PlayerCard -> SortArea.SortIndex -> Bool -> SortMoveButtonSet -> Html Msg
        renderSortCard playerCard sortIndex markedForDiscard sortMoveButtonSet =
            div [ class "flex flex-col items-center mr-4 space-y-2" ]
                [ renderPlayerCard playerCard False
                , div
                    [ class <|
                        if markedForDiscard then
                            ""

                        else
                            "opacity-0"
                    ]
                    [ text "Marked for discard" ]
                , div [ class "flex" ]
                    [ renderLeftArrow sortMoveButtonSet
                    , button [ class transparentButton, onClick (SortToggleDiscard sortIndex) ] [ text "Toggle discard" ]
                    , renderRightArrow sortMoveButtonSet
                    ]
                ]

        renderCards : List (Html Msg)
        renderCards =
            case sortArea of
                SortArea.OneRevealed first SortArea.DiscardZeroOfOne ->
                    [ renderSortCard first SortArea.First False SortMoveButtonsNone
                    ]

                SortArea.OneRevealed first SortArea.DiscardOneOfOne ->
                    [ renderSortCard first SortArea.First True SortMoveButtonsNone
                    ]

                SortArea.TwoRevealed first second SortArea.DiscardZeroOfTwo ->
                    [ renderSortCard first SortArea.First False (SortMoveButtonsRight SortArea.OneToTwo)
                    , renderSortCard second SortArea.Second False (SortMoveButtonsLeft SortArea.OneToTwo)
                    ]

                SortArea.TwoRevealed first second SortArea.DiscardOneOfTwo ->
                    [ renderSortCard first SortArea.First True SortMoveButtonsNone
                    , renderSortCard second SortArea.Second False SortMoveButtonsNone
                    ]

                SortArea.TwoRevealed first second SortArea.DiscardTwoOfTwo ->
                    [ renderSortCard first SortArea.First False SortMoveButtonsNone
                    , renderSortCard second SortArea.Second True SortMoveButtonsNone
                    ]

                SortArea.ThreeRevealed first second third SortArea.DiscardZeroOfThree ->
                    [ renderSortCard first SortArea.First False (SortMoveButtonsRight SortArea.OneToTwo)
                    , renderSortCard second SortArea.Second False SortMoveButtonsBoth
                    , renderSortCard third SortArea.Third False (SortMoveButtonsLeft SortArea.TwoToThree)
                    ]

                SortArea.ThreeRevealed first second third SortArea.DiscardOneOfThree ->
                    [ renderSortCard first SortArea.First True SortMoveButtonsNone
                    , renderSortCard second SortArea.Second False (SortMoveButtonsRight SortArea.TwoToThree)
                    , renderSortCard third SortArea.Third False (SortMoveButtonsLeft SortArea.TwoToThree)
                    ]

                SortArea.ThreeRevealed first second third SortArea.DiscardTwoOfThree ->
                    [ renderSortCard first SortArea.First False (SortMoveButtonsRight SortArea.OneToThree)
                    , renderSortCard second SortArea.Second True SortMoveButtonsNone
                    , renderSortCard third SortArea.Third False (SortMoveButtonsLeft SortArea.OneToThree)
                    ]

                SortArea.ThreeRevealed first second third SortArea.DiscardThreeOfThree ->
                    [ renderSortCard first SortArea.First False (SortMoveButtonsRight SortArea.OneToTwo)
                    , renderSortCard second SortArea.Second False (SortMoveButtonsLeft SortArea.OneToTwo)
                    , renderSortCard third SortArea.Third True SortMoveButtonsNone
                    ]

        renderButtons : Html Msg
        renderButtons =
            div [ class "flex flex-col justify-center items-center space-y-2 mx-8 mb-4" ]
                [ button [ class transparentButton, onClick SortReveal ] [ text "Reveal" ]
                , button [ class transparentButton, onClick SortFinish ] [ text "Finish" ]
                ]
    in
    div [ class "flex flex-wrap" ] (renderButtons :: renderCards)


renderFightingHazard : CommonState -> FightArea -> HazardCard -> FightView -> Html Msg
renderFightingHazard commonState fightArea hazard fightView =
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , div [ class rightColClasses ]
            [ div [ class "flex flex-col h-32 items-center justify-center" ]
                [ div [ class "text-3xl font-bold" ] [ text "Fight Hazard" ]
                ]
            , renderFightDash
                { canDraw = PlayerDeck.canDraw commonState.playerDeck
                , fightArea = fightArea
                , renderEnemy = renderHazard commonState.phase hazard
                , freeCards = HazardCard.getFreeCards hazard
                , enemyStrength = HazardCard.getStrength hazard
                , endFightResult = attemptEndFightHazard commonState.phase fightArea hazard
                }
            , case fightView of
                SortView sortArea ->
                    renderSortArea sortArea

                _ ->
                    div [ class "flex flex-wrap" ] (List.indexedMap (renderPlayedCard commonState fightArea fightView) (FightArea.getPlayedCards fightArea))
            ]
        ]



-- FinalShowdown


renderPirate : PirateCard -> Html Msg
renderPirate pirate =
    div [ class "flex flex-col bg-orange-300 h-32 w-64 p-2 border-8 border-orange-600 rounded border-white text-orange-800 relative" ]
        [ div [ class "font-bold mb-2" ] [ text "Pirate" ]
        , div [ class "flex items-center mb-1" ]
            [ div [ class "text-sm mr-2" ] [ text "Strength: " ]
            , div [ class statCircle ]
                [ text <| String.fromInt <| PirateCard.getStrength pirate
                ]
            ]
        , div [ class "flex items-center" ]
            [ div [ class "text-sm mr-2" ] [ text "Free cards: " ]
            , div [ class statWhite ]
                [ text <| String.fromInt <| PirateCard.getFreeCards pirate
                ]
            ]
        ]


renderFinalShowdown : CommonState -> FightArea -> PirateCard -> FightView -> Html Msg
renderFinalShowdown commonState fightArea pirate fightView =
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , div [ class rightColClasses ]
            [ div [ class "flex flex-col h-32 items-center justify-center" ]
                [ div [ class "text-3xl font-bold" ] [ text "Fight Hazard" ]
                ]
            , renderFightDash
                { canDraw = PlayerDeck.canDraw commonState.playerDeck
                , fightArea = fightArea
                , renderEnemy = renderPirate pirate
                , freeCards = PirateCard.getFreeCards pirate
                , enemyStrength = PirateCard.getStrength pirate
                , endFightResult = attemptEndFinalShowdown fightArea pirate
                }
            , div [ class "flex flex-wrap" ] (List.indexedMap (renderPlayedCard commonState fightArea fightView) (FightArea.getPlayedCards fightArea))
            ]
        ]



-- PlayerWon


renderPlayerWon : CommonState -> HazardCard -> Html Msg
renderPlayerWon commonState reward =
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , div [ class rightColClasses ]
            [ div [ class "flex flex-col h-32 items-center justify-center" ]
                [ div [ class "text-3xl font-bold" ] [ text "Collect Reward" ]
                ]
            , div [ class "flex flex-col items-center" ]
                [ renderPlayerCard (PlayerCard.fromHazardCard reward) False ]
            , div [ class "flex flex-col items-center" ]
                [ button [ class standardButton, onClick AcceptWin ] [ text "Accept" ] ]
            ]
        ]



-- PlayerLost


renderPlayerLost : CommonState -> HealthLost -> SelectionList PlayerCard -> Html Msg
renderPlayerLost commonState healthLost playerCardList =
    let
        numSelected : Int
        numSelected =
            SelectionList.countSelected playerCardList

        renderSelectableCard : Int -> Bool -> PlayerCard -> Html Msg
        renderSelectableCard index isSelected playerCard =
            div [ class "flex flex-col justify-center" ]
                [ div [ class "relative mr-4 mb-4" ]
                    [ renderPlayerCard playerCard False
                    , if isSelected then
                        div [ class "absolute top-0 left-0 w-full h-full opacity-50 bg-black" ] []

                      else
                        div [] []
                    ]
                , div []
                    [ text <|
                        if isSelected then
                            "selected"

                        else
                            "not selected"
                    ]
                , button [ class standardButton, onClick (ToggleLossDestroy index) ]
                    [ text <|
                        if isSelected then
                            "Deselect"

                        else
                            "Select"
                    ]
                ]
    in
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , div [ class rightColClasses ]
            [ div [ class "flex flex-col h-32 items-center justify-center" ]
                [ div [ class "text-3xl font-bold" ] [ text "Sacrifice Cards" ]
                ]
            , div [ class "flex flex-col items-center" ]
                [ div [] [ text "You have lost...but you might have unlearned some bad habits." ] ]
            , div [ class "flex flex-col items-center" ]
                [ div [] [ text <| String.fromInt numSelected ++ "/" ++ String.fromInt healthLost ++ " cards sacrificed" ] ]
            , div [ class "flex flex-col items-center" ]
                [ button [ class standardButton, onClick AcceptLoss ] [ text "Accept" ] ]
            , div [ class "flex flex-wrap" ]
                (SelectionList.map renderSelectableCard playerCardList)
            ]
        ]



-- VIEW


leftColClasses : String
leftColClasses =
    "flex flex-col bg-gray-900 w-1/5 rounded shadow p-4 space-y-12"


rightColClasses : String
rightColClasses =
    "flex flex-col w-4/5 bg-gray-900 rounded shadow p-4 space-y-12"


view : Model -> Html Msg
view model =
    div [ class "w-screen h-screen text-white flex flex-col justify-start items-center" ]
        [ div [ class "w-3/4 flex-grow flex flex-col py-8" ]
            [ div [ class "flex items-end border border-t-0 border-r-0 border-l-0 border-white mb-8" ]
                [ h1 [ class "text-4xl font-semibold" ] [ text "Friday" ]
                , h3 [ class "pl-6" ]
                    [ span [ class "line-through" ] [ text "Partially" ]
                    , span [] [ text " Fully functional game" ]
                    ]
                ]
            , case model of
                GameOver Win ->
                    h2 [ class "text-xl" ] [ text "You are winner!" ]

                GameOver Loss ->
                    h2 [ class "text-xl" ] [ text "You are loser!" ]

                GameInProgress (HazardSelection commonState hazardOption) ->
                    renderHazardSelection commonState hazardOption

                GameInProgress (FightingHazard commonState fightArea hazard fightView) ->
                    renderFightingHazard commonState fightArea hazard fightView

                GameInProgress (ResolvingFight commonState (PlayerWon reward)) ->
                    renderPlayerWon commonState reward

                GameInProgress (ResolvingFight commonState (PlayerLost healthLost playerCardList)) ->
                    renderPlayerLost commonState healthLost playerCardList

                GameInProgress (FinalShowdown commonState fightArea pirate fightView) ->
                    renderFinalShowdown commonState fightArea pirate fightView
            ]
        ]
