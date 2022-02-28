{-# LANGUAGE PatternSynonyms #-}

module Chapter3.BidirectionalRange (Range()) where

    data Range = Range Integer Integer deriving (Show)

    -- smart constructor
    range :: Integer -> Integer -> Range
    range a b = if a <= b then Range a b else error "a must be <= b"

    -- allowed by PatternSynonyms. this is a bidirectional pattern.
    pattern R :: Integer -> Integer -> Range -- type signature coincides with constructor
    pattern R a b <- Range a b -- matcher: I declare that: (matching R a b) =:= (writing a pattern match of the form Range a b)
        where R a b = range a b -- builder: using R x y =:= calling the range function
    -- this solves the issue of hidden constructor, while keeping the ability to pattern match.