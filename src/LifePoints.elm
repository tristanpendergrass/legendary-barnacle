module LifePoints exposing (Counter, createCounter, decrementCounter, getValue, incrementCounter)


clampLife : Int -> Int
clampLife =
    clamp 0 22


type Counter
    = Counter Int


createCounter : Int -> Counter
createCounter initialCount =
    Counter (clampLife initialCount)


decrementCounter : Int -> Counter -> Maybe Counter
decrementCounter amount (Counter oldValue) =
    let
        newValue : Int
        newValue =
            oldValue - amount
    in
    if newValue > -1 then
        Just (Counter newValue)

    else
        Nothing


getValue : Counter -> Int
getValue (Counter value) =
    value


incrementCounter : Counter -> Counter
incrementCounter (Counter oldValue) =
    let
        newValue : Int
        newValue =
            clampLife (oldValue + 1)
    in
    Counter newValue
