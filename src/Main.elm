module Main exposing (main)

import AgingCard exposing (AgingCard)
import Browser
import Deck exposing (Deck)
import FightArea exposing (FightArea)
import FightStats exposing (SpecialAbility(..))
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
    | PlayerLost Int (SelectionList PlayerCard)


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


{-| Returns the game state to hazard selection with the phase updated, or moves to the final showdown if already in PhaseRed
-}
handlePhaseComplete : Maybe HazardCard -> CommonState -> GameState
handlePhaseComplete maybeHazardCard incompleteCommonState =
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
            { incompleteCommonState | hazardDeck = Deck.discard leftoverCards incompleteCommonState.hazardDeck }

        toHazardSelection : Phase -> GameState
        toHazardSelection phase =
            let
                ( shuffledHazards, seedAfterReshuffle ) =
                    Random.step (Deck.reshuffle commonState.hazardDeck) commonState.seed

                drawTwiceResultAndSeed : ( Deck.DrawTwiceResult HazardCard, Random.Seed )
                drawTwiceResultAndSeed =
                    Random.step (Deck.drawTwiceWithReshuffle shuffledHazards) seedAfterReshuffle

                ( drawTwiceResult, seedAfterDraw ) =
                    drawTwiceResultAndSeed
            in
            case drawTwiceResult of
                Deck.NothingDrawn ->
                    -- Should never happen that all hazards run out, but if it does move directly to final showdown
                    FinalShowdown { commonState | phase = PhaseRed }

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


drawCard : CommonState -> Maybe ( PlayerCard, CommonState )
drawCard commonState =
    let
        { playerDeck, seed, agingCards } =
            commonState

        agingCardsAsPlayerCards : List PlayerCard
        agingCardsAsPlayerCards =
            List.map PlayerCard.fromAgingCard agingCards
    in
    case Random.step (Deck.drawWithReshuffle playerDeck agingCardsAsPlayerCards) seed of
        ( Nothing, _ ) ->
            Nothing

        ( Just ( drawnCard, newPlayerDeck, newAgingCards ), newSeed ) ->
            Just ( drawnCard, { commonState | playerDeck = newPlayerDeck, seed = newSeed, agingCards = newAgingCards } )


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
            ( GameInProgress (handlePhaseComplete (Just card) commonState), Cmd.none )

        -- Fight
        ( Draw, FightingHazard commonState fightArea NormalFightView ) ->
            case drawCard commonState of
                Nothing ->
                    noOp

                Just ( drawnCard, newCommonState ) ->
                    let
                        newFightArea : FightArea
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
                -- Player lost, strengthDifference > 0
                case LifePoints.decrementCounter strengthDifference commonState.lifePoints of
                    Nothing ->
                        ( GameOver, Cmd.none )

                    Just newLifePoints ->
                        let
                            newHazardDeck : Deck HazardCard
                            newHazardDeck =
                                Deck.discard [ hazard ] hazardDeck

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
                    { commonState | playerDeck = Deck.discard [ PlayerCard.fromHazardCard hazardCard ] commonState.playerDeck }
            in
            case Deck.drawTwice commonState.hazardDeck of
                Deck.NothingDrawn ->
                    ( GameInProgress (handlePhaseComplete Nothing newCommonState), Cmd.none )

                Deck.DrewOne newHazardDeck card ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (One card)), Cmd.none )

                Deck.DrewTwo newHazardDeck first second ->
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
                    { commonState | playerDeck = Deck.discard (SelectionList.getUnselected selectionList) commonState.playerDeck }
            in
            case Deck.drawTwice commonState.hazardDeck of
                Deck.NothingDrawn ->
                    ( GameInProgress (handlePhaseComplete Nothing newCommonState), Cmd.none )

                Deck.DrewOne newHazardDeck card ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (One card)), Cmd.none )

                Deck.DrewTwo newHazardDeck first second ->
                    ( GameInProgress (HazardSelection { newCommonState | hazardDeck = newHazardDeck } (Two first second)), Cmd.none )

        _ ->
            noOp


type alias ResolveAbilityArg =
    { ability : SpecialAbility
    , setCardInUse : FightArea
    , setCardUsed : FightArea
    , commonState : CommonState
    , fightArea : FightArea
    }


resolveAbility : ResolveAbilityArg -> GameState
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

                newFightArea : FightArea
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
