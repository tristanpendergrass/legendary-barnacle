module Main exposing (main)

import AgingCard exposing (AgingCard)
import Browser
import FightArea exposing (FightArea)
import FightStats exposing (SpecialAbility(..))
import HazardCard exposing (HazardCard)
import HazardDeck exposing (HazardDeck)
import Html exposing (Html, a, button, div, h1, h2, h3, img, li, span, text, ul)
import Html.Attributes exposing (class, src)
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


type ResolvingState
    = PlayerWon HazardCard
    | PlayerLost Int (SelectionList PlayerCard)


type alias AndAnother =
    Bool


type FightView
    = NormalFightView
    | SortView (SortArea PlayerCard)
    | SelectCopyView
    | SelectDoubleView
    | SelectBelowTheStackView Int
    | SelectExchangeView Int AndAnother
    | SelectDestroyView Int


type GameState
    = HazardSelection CommonState (OneOrTwo HazardCard)
    | FightingHazard CommonState (FightArea HazardCard) FightView
    | ResolvingFight CommonState ResolvingState
    | FinalShowdown CommonState (FightArea PirateCard) FightView


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
            , phase = PhaseRed
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
    | SortFinish
    | SortChangeOrder SortArea.ChangeOrderType
    | SortDiscard SortArea.SortIndex
    | SortReveal
    | SelectCopy Int
    | SelectDouble Int
    | SelectBelowTheStack Int
    | SelectExchange Int
    | SelectDestroy Int
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


type DrawCardNormallyResult a
    = CantDrawNormally
    | DrawingKillsPlayer
    | DrewCardNormally { newCommonState : CommonState, newFightArea : FightArea a }


attemptDrawNormally : CommonState -> FightArea a -> Int -> DrawCardNormallyResult a
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
                        newFightArea : FightArea a
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
        ( DrawNormally, FightingHazard commonState fightArea NormalFightView ) ->
            let
                freeDraws : Int
                freeDraws =
                    fightArea
                        |> FightArea.getEnemy
                        |> HazardCard.getFreeCards
            in
            case attemptDrawNormally commonState fightArea freeDraws of
                DrawingKillsPlayer ->
                    ( GameOver, Cmd.none )

                CantDrawNormally ->
                    noOp

                DrewCardNormally { newCommonState, newFightArea } ->
                    ( GameInProgress (FightingHazard newCommonState newFightArea NormalFightView), Cmd.none )

        ( DrawNormally, FinalShowdown commonState fightArea NormalFightView ) ->
            let
                freeDraws : Int
                freeDraws =
                    fightArea
                        |> FightArea.getEnemy
                        |> PirateCard.getFreeCards
            in
            case attemptDrawNormally commonState fightArea freeDraws of
                DrawingKillsPlayer ->
                    ( GameOver, Cmd.none )

                CantDrawNormally ->
                    noOp

                DrewCardNormally { newCommonState, newFightArea } ->
                    ( GameInProgress (FinalShowdown newCommonState newFightArea NormalFightView), Cmd.none )

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

        ( EndFight, FinalShowdown commonState fightArea NormalFightView ) ->
            if FightArea.hasUnusedAgingCards fightArea then
                noOp

            else
                let
                    { playerDeck, pirateStatus } =
                        commonState

                    playerStrength : Int
                    playerStrength =
                        FightArea.getPlayerStrength fightArea

                    pirate : PirateCard
                    pirate =
                        FightArea.getEnemy fightArea

                    pirateStrength : Int
                    pirateStrength =
                        PirateCard.getStrength pirate

                    strengthDifference : Int
                    strengthDifference =
                        pirateStrength - playerStrength

                    playerWon : Bool
                    playerWon =
                        strengthDifference <= 0
                in
                if playerWon then
                    case pirateStatus of
                        BothPiratesAlive ->
                            let
                                discardedCards : List PlayerCard
                                discardedCards =
                                    FightArea.getCards fightArea

                                newPlayerDeck : PlayerDeck
                                newPlayerDeck =
                                    PlayerDeck.discard discardedCards playerDeck

                                newCommonState : CommonState
                                newCommonState =
                                    { commonState | playerDeck = newPlayerDeck, pirateStatus = OnePirateDefeated }

                                newGameState : GameState
                                newGameState =
                                    FinalShowdown newCommonState (FightArea.createFightArea commonState.pirateTwo) NormalFightView
                            in
                            ( GameInProgress newGameState, Cmd.none )

                        OnePirateDefeated ->
                            ( GameOver, Cmd.none )

                else
                    -- Player isn't allowed to surrender the final showdown
                    noOp

        ( UseAbility index, FightingHazard commonState fightArea NormalFightView ) ->
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
                        ( GameInProgress (FightingHazard newCommonState newFightArea newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( UseAbility index, FinalShowdown commonState fightArea NormalFightView ) ->
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
                        ( GameInProgress (FinalShowdown newCommonState newFightArea newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( CancelAbilitiesInUse, FightingHazard commonState fightArea SelectCopyView ) ->
            ( GameInProgress (FightingHazard commonState (FightArea.undoAllInUse fightArea) NormalFightView), Cmd.none )

        ( CancelAbilitiesInUse, FinalShowdown commonState fightArea SelectCopyView ) ->
            ( GameInProgress (FinalShowdown commonState (FightArea.undoAllInUse fightArea) NormalFightView), Cmd.none )

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

        ( SortFinish, FinalShowdown commonState fightArea (SortView sortArea) ) ->
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

                newFightArea : FightArea PirateCard
                newFightArea =
                    FightArea.setInUseToUsed fightArea
            in
            ( GameInProgress (FinalShowdown newCommonState newFightArea NormalFightView), Cmd.none )

        ( SortChangeOrder changeOrderType, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.changeOrder changeOrderType sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea (SortView newSortArea)), Cmd.none )

        ( SortChangeOrder changeOrderType, FinalShowdown commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.changeOrder changeOrderType sortArea
            in
            ( GameInProgress (FinalShowdown commonState fightArea (SortView newSortArea)), Cmd.none )

        ( SortDiscard discardType, FightingHazard commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.toggleDiscard discardType sortArea
            in
            ( GameInProgress (FightingHazard commonState fightArea (SortView newSortArea)), Cmd.none )

        ( SortDiscard discardType, FinalShowdown commonState fightArea (SortView sortArea) ) ->
            let
                newSortArea : SortArea PlayerCard
                newSortArea =
                    SortArea.toggleDiscard discardType sortArea
            in
            ( GameInProgress (FinalShowdown commonState fightArea (SortView newSortArea)), Cmd.none )

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

        ( SortReveal, FinalShowdown commonState fightArea (SortView sortArea) ) ->
            case ( SortArea.attemptReveal sortArea, drawCard commonState ) of
                ( Just onReveal, Just ( drawnCard, newCommonState ) ) ->
                    let
                        newSortArea : SortArea PlayerCard
                        newSortArea =
                            onReveal drawnCard
                    in
                    ( GameInProgress (FinalShowdown newCommonState fightArea (SortView newSortArea)), Cmd.none )

                _ ->
                    noOp

        ( SelectCopy index, FightingHazard commonState fightArea SelectCopyView ) ->
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
                        ( GameInProgress (FightingHazard newCommonState newFightArea newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( SelectCopy index, FinalShowdown commonState fightArea SelectCopyView ) ->
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
                        ( GameInProgress (FinalShowdown newCommonState newFightArea newFightView), Cmd.none )
                    )
                |> Result.withDefault noOp

        ( SelectDouble index, FightingHazard commonState fightArea SelectDoubleView ) ->
            case FightArea.attemptDouble index fightArea of
                Just newFightArea ->
                    ( GameInProgress (FightingHazard commonState (FightArea.setInUseToUsed newFightArea) NormalFightView), Cmd.none )

                Nothing ->
                    noOp

        ( SelectDouble index, FinalShowdown commonState fightArea SelectDoubleView ) ->
            case FightArea.attemptDouble index fightArea of
                Just newFightArea ->
                    ( GameInProgress (FinalShowdown commonState (FightArea.setInUseToUsed newFightArea) NormalFightView), Cmd.none )

                Nothing ->
                    noOp

        ( SelectBelowTheStack index, FightingHazard commonState fightArea (SelectBelowTheStackView belowTheStackIndex) ) ->
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
                        ( GameInProgress (FightingHazard newCommonState (onExchange drawnCard) NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectBelowTheStack index, FinalShowdown commonState fightArea (SelectBelowTheStackView belowTheStackIndex) ) ->
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
                        ( GameInProgress (FinalShowdown newCommonState (onExchange drawnCard) NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectExchange index, FightingHazard commonState fightArea (SelectExchangeView exchangeIndex andAnother) ) ->
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

                            newFightArea : FightArea HazardCard
                            newFightArea =
                                if andAnother then
                                    onExchange drawnCard

                                else
                                    FightArea.setInUseToUsed (onExchange drawnCard)
                        in
                        ( GameInProgress (FightingHazard newCommonState newFightArea newFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectExchange index, FinalShowdown commonState fightArea (SelectExchangeView exchangeIndex andAnother) ) ->
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

                            newFightArea : FightArea PirateCard
                            newFightArea =
                                if andAnother then
                                    onExchange drawnCard

                                else
                                    FightArea.setInUseToUsed (onExchange drawnCard)
                        in
                        ( GameInProgress (FinalShowdown newCommonState newFightArea newFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectDestroy index, FightingHazard commonState fightArea (SelectDestroyView destroyIndex) ) ->
            if index == destroyIndex then
                noOp

            else
                case FightArea.attemptDestroy index fightArea of
                    Just onDestroy ->
                        let
                            newFightArea : FightArea HazardCard
                            newFightArea =
                                FightArea.setInUseToUsed onDestroy
                        in
                        ( GameInProgress (FightingHazard commonState newFightArea NormalFightView), Cmd.none )

                    Nothing ->
                        noOp

        ( SelectDestroy index, FinalShowdown commonState fightArea (SelectDestroyView destroyIndex) ) ->
            if index == destroyIndex then
                noOp

            else
                case FightArea.attemptDestroy index fightArea of
                    Just onDestroy ->
                        let
                            newFightArea : FightArea PirateCard
                            newFightArea =
                                FightArea.setInUseToUsed onDestroy
                        in
                        ( GameInProgress (FinalShowdown commonState newFightArea NormalFightView), Cmd.none )

                    Nothing ->
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
    , index : Int
    , setCardInUse : FightArea a
    , setCardUsed : FightArea a
    , commonState : CommonState
    , fightArea : FightArea a
    }


attemptResolveAbility : ResolveAbilityArg a -> Result String ( CommonState, FightArea a, FightView )
attemptResolveAbility { ability, index, setCardInUse, setCardUsed, commonState, fightArea } =
    case ability of
        PlusOneLife ->
            let
                newLifePoints : LifePoints.Counter
                newLifePoints =
                    LifePoints.incrementCounter commonState.lifePoints

                newCommonState : CommonState
                newCommonState =
                    { commonState | lifePoints = newLifePoints }

                newFightArea : FightArea a
                newFightArea =
                    setCardUsed
            in
            Ok ( newCommonState, newFightArea, NormalFightView )

        SortThree ->
            case drawCard commonState of
                Nothing ->
                    Err "Must be able to draw card"

                Just ( drawnCard, newCommonState ) ->
                    Ok ( newCommonState, setCardInUse, SortView (SortArea.create drawnCard) )

        Copy ->
            Ok ( commonState, setCardInUse, SelectCopyView )

        Double ->
            Ok ( commonState, setCardInUse, SelectDoubleView )

        BelowTheStack ->
            Ok ( commonState, setCardInUse, SelectBelowTheStackView index )

        ExchangeTwo ->
            Ok ( commonState, setCardInUse, SelectExchangeView index True )

        Destroy ->
            Ok ( commonState, setCardInUse, SelectDestroyView index )

        PhaseMinusOne ->
            if commonState.phase == PhaseGreen then
                Err "Phase cannot go below Green"

            else
                Ok ( commonState, FightArea.setPhaseMinusOne setCardUsed, NormalFightView )

        _ ->
            Debug.todo "Implement missing ability"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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
    div [ class "flex flex-col bg-gray-900 w-1/5 rounded shadow p-4 space-y-12" ]
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


renderHazard : Phase -> HazardCard -> Html Msg
renderHazard phase hazardCard =
    div [ class "flex flex-col bg-orange-300 h-32 w-64 p-2 border-8 border-orange-600 rounded border-white text-orange-800" ]
        [ div [ class "font-bold mb-2" ] [ text (HazardCard.getTitle hazardCard) ]
        , div [ class "flex items-center mb-1" ]
            [ div [ class "text-sm mr-2" ] [ text "Strength: " ]
            , case phase of
                PhaseGreen ->
                    div [ class "bg-green-600 text-gray-100 border-gray-10 border font-semibold text-sm px-4 py-0 rounded-sm" ]
                        [ text <| String.fromInt <| HazardCard.getGreenValue hazardCard
                        ]

                PhaseYellow ->
                    div [ class "bg-yellow-500 text-gray-100 border-gray-100 border font-semibold text-sm px-4 py-0 rounded-sm" ]
                        [ text <| String.fromInt <| HazardCard.getYellowValue hazardCard
                        ]

                PhaseRed ->
                    div [ class "bg-red-600 text-gray-100 border-gray-100 border font-semibold text-sm px-4 py-0 rounded-sm" ]
                        [ text <| String.fromInt <| HazardCard.getRedValue hazardCard
                        ]
            ]
        , div [ class "flex items-center" ]
            [ div [ class "text-sm mr-2" ] [ text "Free cards: " ]
            , div [ class "bg-gray-100 text-gray-900 border-gray-900 border font-semibold text-sm px-4 py-0 rounded-sm" ]
                [ text <| String.fromInt <| HazardCard.getFreeCards hazardCard
                ]
            ]
        ]


renderHazardChoice : Phase -> OneOrTwo HazardCard -> Html Msg
renderHazardChoice phase hazardOption =
    let
        renderChooseButton : String -> Msg -> Html Msg
        renderChooseButton buttonLabel handleClick =
            button [ class "bg-gray-100 hover:bg-gray-300 font-bold text-gray-800 rounded py-2 px-4 rounded", onClick handleClick ]
                [ text buttonLabel
                ]
    in
    div [ class "flex flex-col flex-grow bg-gray-900 rounded shadow p-4 space-y-12" ]
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
                        -- TODO: test this
                        [ renderHazard phase hazard
                        , renderChooseButton "Choose" ChooseSingleHazard
                        ]
                    , div [ class "flex flex-col items-center space-y-4" ]
                        [ renderChooseButton "Skip" ChooseSkipHazard
                        ]
                    ]
        ]


renderHazardSelection : CommonState -> OneOrTwo HazardCard -> Html Msg
renderHazardSelection commonState hazardOption =
    div [ class "flex flex-row flex-grow space-x-8" ]
        [ renderCommonState commonState
        , renderHazardChoice commonState.phase hazardOption
        ]


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
                GameOver ->
                    h2 [ class "text-xl" ] [ text "Game over" ]

                GameInProgress (HazardSelection commonState hazardOption) ->
                    renderHazardSelection commonState hazardOption

                GameInProgress _ ->
                    h2 [ class "text-xl" ] [ text "Game in progress (phase not implemented)" ]
            ]
        ]
