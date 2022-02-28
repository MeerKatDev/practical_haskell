{-# LANGUAGE ViewPatterns #-}

module Chapter3.Range (Range(), range, rangeObs, prettyRange) where
    -- data type, exported without constructor
    data Range = Range Integer Integer deriving (Show)

    -- smart constructor
    range :: Integer -> Integer -> Range
    range a b = if a <= b then Range a b else error "a must be <= b"

    -- all is well, the only problem now is pattern matching behavior
    -- this won't compile: case ... of Range x y ->
    -- since the constructor is hidden.
    -- the solution is using ViewPatterns
    data RangeObs = R Integer Integer deriving (Show)
    -- this observer can now pattern match
    rangeObs :: Range -> RangeObs
    rangeObs (Range a b) = R a b

    -- this should be tested as an outside definition,
    -- but it would complicate things
    prettyRange :: Range -> String
    prettyRange rng = case rng of
        (rangeObs -> R a b) -> "[" ++ show a ++ ", " ++ show b ++ "]"