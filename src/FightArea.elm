module FightArea exposing
    ( FightArea
    , IsDoubled
    , PhaseReduction(..)
    , PlayedCard(..)
    , UsedState(..)
    , attemptCopy
    , attemptDestroy
    , attemptDouble
    , attemptExchange
    , attemptPlayFreeCard
    , attemptUse
    , createFightArea
    , getAbility
    , getAbilityCards
    , getCard
    , getCards
    , getFreeCardsDrawn
    , getPhaseReduction
    , getPlayedCards
    , getPlayerStrength
    , hasUnusedAgingCards
    , isNull
    , playCard
    , reducePhase
    , setCardInUse
    , setCardNotUsed
    , setCardUsed
    , setInUseToUsed
    , undoAllInUse
    , undoubledCardIndexes
    )

import FightStats exposing (SpecialAbility(..))
import List.Extra
import Phase exposing (Phase(..))
import PlayerCard exposing (PlayerCard)


type UsedState
    = NotUsed
    | Used
    | InUse


type alias IsDoubled =
    Bool


type PlayedCard
    = NormalCard PlayerCard IsDoubled
    | AbilityCard PlayerCard UsedState IsDoubled


type PhaseReduction
    = NoPhaseReduction
    | ReducePhaseOnce
    | ReducePhaseTwice


type alias FreeCardsDrawn =
    Int


type FightArea
    = FightArea (List PlayedCard) PhaseReduction FreeCardsDrawn


getPlayedCards : FightArea -> List PlayedCard
getPlayedCards (FightArea cards _ _) =
    cards


toPlayedCard : PlayerCard -> PlayedCard
toPlayedCard card =
    if PlayerCard.hasAbility card then
        AbilityCard card NotUsed False

    else
        NormalCard card False


fromPlayedCard : PlayedCard -> PlayerCard
fromPlayedCard playedCard =
    case playedCard of
        AbilityCard card _ _ ->
            card

        NormalCard card _ ->
            card


createFightArea : FightArea
createFightArea =
    FightArea [] NoPhaseReduction 0


playCard : PlayerCard -> FightArea -> FightArea
playCard card (FightArea cards phaseMinusOne freeCardsDrawn) =
    FightArea (toPlayedCard card :: cards) phaseMinusOne freeCardsDrawn


isStopDrawingCard : PlayedCard -> Bool
isStopDrawingCard playedCard =
    case playedCard of
        AbilityCard playerCard _ _ ->
            playerCard
                |> PlayerCard.getAbility
                |> Maybe.map ((==) StopDrawing)
                |> Maybe.withDefault False

        _ ->
            False


attemptPlayFreeCard : PlayerCard -> Int -> FightArea -> Maybe FightArea
attemptPlayFreeCard card freeCardLimit (FightArea cards phaseMinusOne freeCardsDrawn) =
    if freeCardsDrawn < freeCardLimit && not (List.any isStopDrawingCard cards) then
        Just (FightArea (toPlayedCard card :: cards) phaseMinusOne (freeCardsDrawn + 1))

    else
        Nothing


getPlayedCardStrength : PlayedCard -> Int
getPlayedCardStrength playedCard =
    let
        strength : Int
        strength =
            playedCard
                |> fromPlayedCard
                |> PlayerCard.getStrength
    in
    case playedCard of
        AbilityCard _ _ False ->
            strength

        AbilityCard _ _ True ->
            strength * 2

        NormalCard _ False ->
            strength

        NormalCard _ True ->
            strength * 2


cardIsHighestCardIsNull : PlayedCard -> Bool
cardIsHighestCardIsNull =
    fromPlayedCard
        >> PlayerCard.getAbility
        >> Maybe.map ((==) HighestCardNull)
        >> Maybe.withDefault False


isNull : Int -> FightArea -> Bool
isNull index (FightArea cards _ _) =
    let
        hasHighestCardNull : Bool
        hasHighestCardNull =
            List.any cardIsHighestCardIsNull cards
    in
    if hasHighestCardNull then
        let
            strengths : List Int
            strengths =
                List.map getPlayedCardStrength cards

            ( _, indexOfMax ) =
                List.Extra.indexedFoldl
                    (\i strength ( max, indexOfMaxSoFar ) ->
                        if strength > max then
                            ( strength, i )

                        else
                            ( max, indexOfMaxSoFar )
                    )
                    ( -2, 0 )
                    strengths
        in
        indexOfMax == index

    else
        False


getPlayerStrength : FightArea -> Int
getPlayerStrength (FightArea cards _ _) =
    let
        ( playerStrength, highestCardStrength, hadHighestCardIsNull ) =
            List.foldl
                (\playedCard ( total, highestCard, highestCardIsNull ) ->
                    let
                        strength : Int
                        strength =
                            getPlayedCardStrength playedCard
                    in
                    ( total + strength, Basics.max highestCard strength, highestCardIsNull || cardIsHighestCardIsNull playedCard )
                )
                ( 0, -2, False )
                cards
    in
    if hadHighestCardIsNull then
        playerStrength - highestCardStrength

    else
        playerStrength


getCard : Int -> FightArea -> Maybe PlayerCard
getCard index fightArea =
    fightArea
        |> getPlayedCards
        |> List.Extra.getAt index
        |> Maybe.map fromPlayedCard


getAbility : Int -> FightArea -> Maybe SpecialAbility
getAbility index fightArea =
    getCard index fightArea
        |> Maybe.andThen PlayerCard.getAbility


isInUse : PlayedCard -> Bool
isInUse playedCard =
    case playedCard of
        AbilityCard _ InUse _ ->
            True

        _ ->
            False


setUsedState : UsedState -> Int -> FightArea -> FightArea
setUsedState newUsedState index (FightArea cards phaseMinusOne freeCardsDrawn) =
    let
        mapCard : PlayedCard -> PlayedCard
        mapCard playedCard =
            case playedCard of
                NormalCard _ _ ->
                    playedCard

                AbilityCard card _ isDoubled ->
                    AbilityCard card newUsedState isDoubled

        setUsedIfIndexMatches : Int -> PlayedCard -> PlayedCard
        setUsedIfIndexMatches i playedCard =
            if i == index then
                mapCard playedCard

            else
                playedCard
    in
    FightArea
        (List.indexedMap setUsedIfIndexMatches cards)
        phaseMinusOne
        freeCardsDrawn


setCardUsed : Int -> FightArea -> FightArea
setCardUsed =
    setUsedState Used


setCardInUse : Int -> FightArea -> FightArea
setCardInUse =
    setUsedState InUse


setCardNotUsed : Int -> FightArea -> FightArea
setCardNotUsed =
    setUsedState NotUsed


replaceAtIndex : Int -> List a -> a -> List a
replaceAtIndex index xs x =
    List.concat
        [ List.take index xs
        , [ x ]
        , List.drop (index + 1) xs
        ]


getCardWithUnusedAbility : PlayedCard -> Maybe ( PlayedCard, SpecialAbility )
getCardWithUnusedAbility playedCard =
    case playedCard of
        AbilityCard playerCard NotUsed _ ->
            playerCard
                |> PlayerCard.getAbility
                |> Maybe.map (\ability -> ( playedCard, ability ))

        _ ->
            Nothing


getCardWithAbility : PlayedCard -> Maybe ( PlayedCard, SpecialAbility )
getCardWithAbility playedCard =
    case playedCard of
        AbilityCard playerCard _ _ ->
            playerCard
                |> PlayerCard.getAbility
                |> Maybe.map (\ability -> ( playedCard, ability ))

        _ ->
            Nothing


setInUse : PlayedCard -> PlayedCard
setInUse card =
    case card of
        AbilityCard playerCard _ isDoubled ->
            AbilityCard playerCard InUse isDoubled

        NormalCard _ _ ->
            card


setUsed : PlayedCard -> PlayedCard
setUsed card =
    case card of
        AbilityCard playerCard _ isDoubled ->
            AbilityCard playerCard Used isDoubled

        NormalCard _ _ ->
            card


attemptUse : Int -> FightArea -> Result String ( SpecialAbility, { setCardInUse : FightArea, setCardUsed : FightArea } )
attemptUse index (FightArea playedCards phaseMinusOne freeCardsDrawn) =
    List.Extra.getAt index playedCards
        |> Maybe.andThen getCardWithUnusedAbility
        |> Maybe.map
            (\( playedCard, ability ) ->
                Ok
                    ( ability
                    , { setCardInUse = FightArea (replaceAtIndex index playedCards (setInUse playedCard)) phaseMinusOne freeCardsDrawn
                      , setCardUsed = FightArea (replaceAtIndex index playedCards (setUsed playedCard)) phaseMinusOne freeCardsDrawn
                      }
                    )
            )
        |> Maybe.withDefault (Err "Can't use card")


attemptCopy : Int -> FightArea -> Result String ( SpecialAbility, { setCardInUse : FightArea, setCardUsed : FightArea } )
attemptCopy index (FightArea playedCards phaseMinusOne freeCardsDrawn) =
    List.Extra.getAt index playedCards
        |> Maybe.andThen getCardWithAbility
        |> Maybe.map
            (\( playedCard, ability ) ->
                Ok
                    ( ability
                    , { setCardInUse = FightArea (replaceAtIndex index playedCards (setInUse playedCard)) phaseMinusOne freeCardsDrawn
                      , setCardUsed = FightArea (replaceAtIndex index playedCards (setUsed playedCard)) phaseMinusOne freeCardsDrawn
                      }
                    )
            )
        |> Maybe.withDefault (Err "Can't copy card")


setInUseToUsed : FightArea -> FightArea
setInUseToUsed (FightArea cards phaseMinusOne freeCardsDrawn) =
    let
        setInUseCardToUsed : PlayedCard -> PlayedCard
        setInUseCardToUsed playedCard =
            case playedCard of
                AbilityCard card InUse isDoubled ->
                    AbilityCard card Used isDoubled

                _ ->
                    playedCard
    in
    FightArea
        (List.map setInUseCardToUsed cards)
        phaseMinusOne
        freeCardsDrawn


getCards : FightArea -> List PlayerCard
getCards =
    getPlayedCards >> List.map fromPlayedCard


undoAllInUse : FightArea -> FightArea
undoAllInUse (FightArea cards phaseMinusOne freeCardsDrawn) =
    let
        setInUseCardToUnused : PlayedCard -> PlayedCard
        setInUseCardToUnused card =
            case card of
                AbilityCard abilityCard InUse isDoubled ->
                    AbilityCard abilityCard NotUsed isDoubled

                _ ->
                    card

        newCards : List PlayedCard
        newCards =
            List.map setInUseCardToUnused cards
    in
    FightArea newCards phaseMinusOne freeCardsDrawn


attemptDoublePlayedCard : PlayedCard -> Maybe PlayedCard
attemptDoublePlayedCard card =
    case card of
        AbilityCard playerCard useState False ->
            Just (AbilityCard playerCard useState True)

        NormalCard playerCard False ->
            Just (NormalCard playerCard True)

        _ ->
            Nothing


attemptDouble : Int -> FightArea -> Maybe FightArea
attemptDouble index (FightArea cards phaseMinusOne freeCardsDrawn) =
    List.Extra.getAt index cards
        |> Maybe.andThen attemptDoublePlayedCard
        |> Maybe.map
            (\doubledCard ->
                FightArea (replaceAtIndex index cards doubledCard) phaseMinusOne freeCardsDrawn
            )


getOnExchange : FightArea -> Int -> PlayerCard -> FightArea
getOnExchange (FightArea cards phaseMinusOne freeCardsDrawn) index newElement =
    let
        newCards : List PlayedCard
        newCards =
            replaceAtIndex index cards (toPlayedCard newElement)
    in
    FightArea newCards phaseMinusOne freeCardsDrawn


attemptExchange : Int -> FightArea -> Maybe ( PlayerCard, PlayerCard -> FightArea )
attemptExchange index (FightArea cards phaseMinusOne freeCardsDrawn) =
    List.Extra.getAt index cards
        |> Maybe.map
            (\card ->
                ( fromPlayedCard card, getOnExchange (FightArea cards phaseMinusOne freeCardsDrawn) index )
            )


attemptDestroy : Int -> FightArea -> Maybe FightArea
attemptDestroy index (FightArea cards phaseMinusOne freeCardsDrawn) =
    let
        maybeNewCards : Maybe (List PlayedCard)
        maybeNewCards =
            if index >= 0 && index < List.length cards then
                Just (List.take index cards ++ List.drop (index + 1) cards)

            else
                Nothing
    in
    Maybe.map (\newCards -> FightArea newCards phaseMinusOne freeCardsDrawn) maybeNewCards


hasUnusedAgingCards : FightArea -> Bool
hasUnusedAgingCards (FightArea cards _ _) =
    let
        isUnusedAgingCard : PlayedCard -> Bool
        isUnusedAgingCard playedCard =
            case playedCard of
                AbilityCard playerCard NotUsed _ ->
                    PlayerCard.isAgingCard playerCard

                _ ->
                    False
    in
    List.any isUnusedAgingCard cards


reducePhase : FightArea -> FightArea
reducePhase (FightArea cards phaseReduction freeCardsDrawn) =
    let
        newPhaseReduction : PhaseReduction
        newPhaseReduction =
            case phaseReduction of
                NoPhaseReduction ->
                    ReducePhaseOnce

                ReducePhaseOnce ->
                    ReducePhaseTwice

                ReducePhaseTwice ->
                    ReducePhaseTwice
    in
    FightArea cards newPhaseReduction freeCardsDrawn


getPhaseReduction : FightArea -> PhaseReduction
getPhaseReduction (FightArea _ phaseReduction _) =
    phaseReduction


getFreeCardsDrawn : FightArea -> Int
getFreeCardsDrawn (FightArea _ _ freeCards) =
    freeCards


getAbilityCards : FightArea -> List PlayerCard
getAbilityCards (FightArea cards _ _) =
    let
        isAbilityCard : PlayedCard -> Bool
        isAbilityCard playedCard =
            case playedCard of
                AbilityCard _ _ _ ->
                    True

                NormalCard _ _ ->
                    False
    in
    cards
        |> List.filter isAbilityCard
        |> List.map fromPlayedCard


undoubledCardIndexes : FightArea -> List Int
undoubledCardIndexes (FightArea cards _ _) =
    let
        isUndoubled : PlayedCard -> Bool
        isUndoubled playedCard =
            case playedCard of
                NormalCard _ isDoubled ->
                    not isDoubled

                AbilityCard _ _ isDoubled ->
                    not isDoubled
    in
    List.Extra.indexedFoldl
        (\index card accum ->
            if isUndoubled card then
                index :: accum

            else
                accum
        )
        []
        cards
