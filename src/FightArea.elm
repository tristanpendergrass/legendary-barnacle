module FightArea exposing
    ( FightArea
    , attemptUse
    , createFightArea
    , getCard
    , getCards
    , getEnemy
    , getPlayerStrength
    , playCard
    , setCardUsed
    , setInUseToUsed
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
    = NormalCard PlayerCard
    | AbilityCard PlayerCard UsedState


type FightArea a
    = FightArea a (List PlayedCard)


getPlayedCards : FightArea a -> List PlayedCard
getPlayedCards (FightArea _ cards) =
    cards


getEnemy : FightArea a -> a
getEnemy (FightArea enemy _) =
    enemy


toPlayedCard : PlayerCard -> PlayedCard
toPlayedCard card =
    if PlayerCard.hasAbility card then
        AbilityCard card NotUsed

    else
        NormalCard card


fromPlayedCard : PlayedCard -> PlayerCard
fromPlayedCard playedCard =
    case playedCard of
        AbilityCard card _ ->
            card

        NormalCard card ->
            card


createFightArea : a -> FightArea a
createFightArea enemy =
    FightArea enemy []


playCard : PlayerCard -> FightArea a -> FightArea a
playCard card (FightArea enemy cards) =
    FightArea enemy (toPlayedCard card :: cards)


getPlayedCardStrength : PlayedCard -> Int
getPlayedCardStrength playedCard =
    playedCard
        |> fromPlayedCard
        |> PlayerCard.getFightingValue


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
        AbilityCard _ InUse ->
            True

        _ ->
            False


setCardUsed : Int -> FightArea a -> FightArea a
setCardUsed index (FightArea enemy cards) =
    let
        useCard : PlayedCard -> PlayedCard
        useCard playedCard =
            case playedCard of
                NormalCard _ ->
                    playedCard

                AbilityCard card _ ->
                    AbilityCard card Used

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


replaceAtIndex : Int -> a -> List a -> List a
replaceAtIndex index x xs =
    List.concat
        [ List.take index xs
        , [ x ]
        , List.drop (index + 1) xs
        ]


getCardWithUnusedAbility : PlayedCard -> Maybe ( PlayerCard, SpecialAbility )
getCardWithUnusedAbility playedCard =
    case playedCard of
        AbilityCard playerCard NotUsed ->
            playerCard
                |> PlayerCard.getAbility
                |> Maybe.map (Tuple.pair playerCard)

        _ ->
            Nothing


attemptUse : Int -> FightArea a -> Maybe ( SpecialAbility, { setCardInUse : FightArea a, setCardUsed : FightArea a } )
attemptUse index (FightArea enemy playedCards) =
    List.Extra.getAt index playedCards
        |> Maybe.andThen getCardWithUnusedAbility
        |> Maybe.map
            (\( playerCard, ability ) ->
                ( ability
                , { setCardInUse = FightArea enemy (replaceAtIndex index (AbilityCard playerCard InUse) playedCards)
                  , setCardUsed = FightArea enemy (replaceAtIndex index (AbilityCard playerCard Used) playedCards)
                  }
                )
            )


setInUseToUsed : FightArea a -> FightArea a
setInUseToUsed (FightArea enemy cards) =
    let
        setInUseCardToUsed : PlayedCard -> PlayedCard
        setInUseCardToUsed playedCard =
            case playedCard of
                AbilityCard card InUse ->
                    AbilityCard card Used

                _ ->
                    playedCard
    in
    FightArea
        enemy
        (List.map setInUseCardToUsed cards)


getCards : FightArea a -> List PlayerCard
getCards =
    getPlayedCards >> List.map fromPlayedCard


undoAllInUse : FightArea a -> FightArea a
undoAllInUse (FightArea enemy cards) =
    let
        setInUseCardToUnused : PlayedCard -> PlayedCard
        setInUseCardToUnused card =
            case card of
                AbilityCard abilityCard InUse ->
                    AbilityCard abilityCard NotUsed

                _ ->
                    card
    in
    cards
        |> List.map setInUseCardToUnused
        |> FightArea enemy
