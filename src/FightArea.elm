module FightArea exposing
    ( FightArea
    , attemptUse
    , createFightArea
    , getCard
    , getCards
    , getHazard
    , getHazardStrength
    , getPlayerStrength
    , playCard
    , setCardUsed
    , setInUseToUsed
    )

import FightStats exposing (SpecialAbility)
import HazardCard exposing (HazardCard)
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


type FightArea
    = FightArea HazardCard (List PlayedCard)


getPlayedCards : FightArea -> List PlayedCard
getPlayedCards (FightArea _ cards) =
    cards


getHazard : FightArea -> HazardCard
getHazard (FightArea hazard _) =
    hazard


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


playedCardCanBeUsed : PlayedCard -> Bool
playedCardCanBeUsed playedCard =
    case playedCard of
        AbilityCard _ NotUsed ->
            True

        _ ->
            False


createFightArea : HazardCard -> FightArea
createFightArea hazardCard =
    FightArea hazardCard []


playCard : PlayerCard -> FightArea -> FightArea
playCard card (FightArea hazardCard cards) =
    FightArea hazardCard (toPlayedCard card :: cards)


getPlayedCardStrength : PlayedCard -> Int
getPlayedCardStrength playedCard =
    playedCard
        |> fromPlayedCard
        |> PlayerCard.getFightingValue


getPlayerStrength : FightArea -> Int
getPlayerStrength =
    getPlayedCards >> List.map getPlayedCardStrength >> List.foldl (+) 0


getHazardStrength : Phase -> FightArea -> Int
getHazardStrength phase (FightArea hazard _) =
    case phase of
        PhaseGreen ->
            HazardCard.getGreenValue hazard

        PhaseYellow ->
            HazardCard.getYellowValue hazard

        PhaseRed ->
            HazardCard.getRedValue hazard


getCard : Int -> FightArea -> Maybe PlayerCard
getCard index fightArea =
    fightArea
        |> getPlayedCards
        |> List.Extra.getAt index
        |> Maybe.map fromPlayedCard


setCardUsed : Int -> FightArea -> FightArea
setCardUsed index (FightArea hazard cards) =
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
            if i == passedIndex then
                useCard playedCard

            else
                playedCard
    in
    FightArea
        hazard
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


attemptUse : Int -> FightArea -> Maybe ( SpecialAbility, { setCardInUse : FightArea, setCardUsed : FightArea } )
attemptUse index (FightArea hazardCard playedCards) =
    List.Extra.getAt index playedCards
        |> Maybe.andThen getCardWithUnusedAbility
        |> Maybe.map
            (\( playerCard, ability ) ->
                ( ability
                , { setCardInUse = FightArea hazardCard (replaceAtIndex index (AbilityCard playerCard InUse) playedCards)
                  , setCardUsed = FightArea hazardCard (replaceAtIndex index (AbilityCard playerCard Used) playedCards)
                  }
                )
            )


setInUseToUsed : FightArea -> FightArea
setInUseToUsed (FightArea hazard cards) =
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
        hazard
        (List.map setInUseCardToUsed cards)


getCards : FightArea -> List PlayerCard
getCards =
    getPlayedCards >> List.map fromPlayedCard
