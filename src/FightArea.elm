module FightArea exposing
    ( FightArea
    , attemptDestroy
    , attemptDouble
    , attemptExchange
    , attemptUse
    , createFightArea
    , getCard
    , getCards
    , getEnemy
    , getPlayerStrength
    , hasUnusedAgingCards
    , isPhaseMinusOne
    , playCard
    , setCardUsed
    , setInUseToUsed
    , setPhaseMinusOne
    , undoAllInUse
    )

import FightStats exposing (SpecialAbility)
import List.Extra
import Phase exposing (Phase(..))
import PlayerCard exposing (PlayerCard)


type UsedState
    = NotUsed
    | Used
    | InUse


type PlayedCard
    = NormalCard PlayerCard Bool
    | AbilityCard PlayerCard UsedState Bool


type alias PhaseMinusOne =
    Bool


type FightArea a
    = FightArea a (List PlayedCard) PhaseMinusOne


getPlayedCards : FightArea a -> List PlayedCard
getPlayedCards (FightArea _ cards _) =
    cards


getEnemy : FightArea a -> a
getEnemy (FightArea enemy _ _) =
    enemy


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


createFightArea : a -> FightArea a
createFightArea enemy =
    FightArea enemy [] False


playCard : PlayerCard -> FightArea a -> FightArea a
playCard card (FightArea enemy cards phaseMinusOne) =
    FightArea enemy (toPlayedCard card :: cards) phaseMinusOne


getPlayedCardStrength : PlayedCard -> Int
getPlayedCardStrength playedCard =
    let
        strength : Int
        strength =
            playedCard
                |> fromPlayedCard
                |> PlayerCard.getFightingValue
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


getPlayerStrength : FightArea a -> Int
getPlayerStrength =
    getPlayedCards >> List.map getPlayedCardStrength >> List.foldl (+) 0


getCard : Int -> FightArea a -> Maybe PlayerCard
getCard index fightArea =
    fightArea
        |> getPlayedCards
        |> List.Extra.getAt index
        |> Maybe.map fromPlayedCard


isInUse : PlayedCard -> Bool
isInUse playedCard =
    case playedCard of
        AbilityCard _ InUse _ ->
            True

        _ ->
            False


setCardUsed : Int -> FightArea a -> FightArea a
setCardUsed index (FightArea enemy cards phaseMinusOne) =
    let
        useCard : PlayedCard -> PlayedCard
        useCard playedCard =
            case playedCard of
                NormalCard _ _ ->
                    playedCard

                AbilityCard card _ isDoubled ->
                    AbilityCard card Used isDoubled

        setUsedIfIndexMatches : Int -> Int -> PlayedCard -> PlayedCard
        setUsedIfIndexMatches passedIndex i playedCard =
            -- Notice the `isInUse playedCard` here to resolve any *other* inUse cards that happen to be in the fight area. This would happen with Copy, I'm not sure of any other scenario.
            if i == passedIndex || isInUse playedCard then
                useCard playedCard

            else
                playedCard
    in
    FightArea
        enemy
        (List.indexedMap (setUsedIfIndexMatches index) cards)
        phaseMinusOne


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


attemptUse : Int -> FightArea a -> Maybe ( SpecialAbility, { setCardInUse : FightArea a, setCardUsed : FightArea a } )
attemptUse index (FightArea enemy playedCards phaseMinusOne) =
    List.Extra.getAt index playedCards
        |> Maybe.andThen getCardWithUnusedAbility
        |> Maybe.map
            (\( playedCard, ability ) ->
                ( ability
                , { setCardInUse = FightArea enemy (replaceAtIndex index playedCards (setInUse playedCard)) phaseMinusOne
                  , setCardUsed = FightArea enemy (replaceAtIndex index playedCards (setUsed playedCard)) phaseMinusOne
                  }
                )
            )


setInUseToUsed : FightArea a -> FightArea a
setInUseToUsed (FightArea enemy cards phaseMinusOne) =
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
        enemy
        (List.map setInUseCardToUsed cards)
        phaseMinusOne


getCards : FightArea a -> List PlayerCard
getCards =
    getPlayedCards >> List.map fromPlayedCard


undoAllInUse : FightArea a -> FightArea a
undoAllInUse (FightArea enemy cards phaseMinusOne) =
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
    FightArea enemy newCards phaseMinusOne


attemptDoublePlayedCard : PlayedCard -> Maybe PlayedCard
attemptDoublePlayedCard card =
    case card of
        AbilityCard playerCard useState False ->
            Just (AbilityCard playerCard useState True)

        NormalCard playerCard False ->
            Just (NormalCard playerCard True)

        _ ->
            Nothing


attemptDouble : Int -> FightArea a -> Maybe (FightArea a)
attemptDouble index (FightArea enemy cards phaseMinusOne) =
    List.Extra.getAt index cards
        |> Maybe.andThen attemptDoublePlayedCard
        |> Maybe.map
            (\doubledCard ->
                FightArea enemy (replaceAtIndex index cards doubledCard) phaseMinusOne
            )


getOnExchange : FightArea a -> Int -> PlayerCard -> FightArea a
getOnExchange (FightArea enemy cards phaseMinusOne) index newElement =
    let
        newCards : List PlayedCard
        newCards =
            replaceAtIndex index cards (toPlayedCard newElement)
    in
    FightArea enemy newCards phaseMinusOne


attemptExchange : Int -> FightArea a -> Maybe ( PlayerCard, PlayerCard -> FightArea a )
attemptExchange index (FightArea enemy cards phaseMinusOne) =
    List.Extra.getAt index cards
        |> Maybe.map
            (\card ->
                ( fromPlayedCard card, getOnExchange (FightArea enemy cards phaseMinusOne) index )
            )


attemptDestroy : Int -> FightArea a -> Maybe (FightArea a)
attemptDestroy index (FightArea enemy cards phaseMinusOne) =
    let
        maybeNewCards : Maybe (List PlayedCard)
        maybeNewCards =
            if index > 0 && index < List.length cards then
                Just (List.take index cards ++ List.drop (index + 1) cards)

            else
                Nothing
    in
    Maybe.map (\newCards -> FightArea enemy newCards phaseMinusOne) maybeNewCards


hasUnusedAgingCards : FightArea a -> Bool
hasUnusedAgingCards (FightArea _ cards _) =
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


setPhaseMinusOne : FightArea a -> FightArea a
setPhaseMinusOne (FightArea enemy cards _) =
    FightArea enemy cards True


isPhaseMinusOne : FightArea a -> Bool
isPhaseMinusOne (FightArea _ _ phaseMinusOne) =
    phaseMinusOne
