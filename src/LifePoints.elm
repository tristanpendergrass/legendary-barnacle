module LifePoints exposing (Counter, createCounter, decrementCounter, getValue, incrementCounter)


clampLife : Int -> Int
clampLife =
    clamp 0 22


type Counter
    = Counter Int


createCounter : Int -> Counter
createCounter initialCount =
    Counter (clampLife initialCount)


decrementCounter : Int -> Counter -> Counter
decrementCounter amount (Counter oldValue) =
    let
        newValue : Int
        newValue =
            clampLife (oldValue - amount)
    in
    Counter newValue


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
