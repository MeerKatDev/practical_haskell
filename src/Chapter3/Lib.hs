{-# LANGUAGE LambdaCase #-}

module Chapter3.Lib where

mapf :: (a -> b) -> [a] -> [b]
mapf _ [] = []
mapf f (x : xs) = (f x) : (mapf f xs)

doubleMapf :: (a -> b -> c) -> [(a, b)] -> [c]
doubleMapf _ [] = []
doubleMapf f ((a, b) : xs) = (f a b) : (doubleMapf f xs)

-- permitted by LambdaCase

sayHello :: [String] -> [String]
sayHello =
    map
        ( \case
            "Alejandro" -> "Welcome, writer"
            name -> "Welcome, " ++ name
        )

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n * x

-- exercise 3.2

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (\x -> x == 1)

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber n = filter (\x -> x == n)

filterNot' :: (a -> Bool) -> [a] -> [a]
filterNot' f = filter $ not . f

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot _ [] = []
filterNot f (x : xs) =
    if (not $ f x)
        then x : (filterNot f xs)
        else (filterNot f xs)

data Person = Person {firstName :: String, lastName :: String} deriving (Show, Eq)

data Client idType
    = GovOrg
        { clientId :: idType
        , clientName :: String
        }
    | Company
        { clientId :: idType
        , clientName :: String
        , companyId :: Integer
        , person :: Person
        , duty :: String
        }
    | Individual
        { clientId :: idType
        , person :: Person
        }
    deriving (Show, Eq)

isGovOrg :: Client a -> Bool
isGovOrg GovOrg{} = True
isGovOrg _ = False

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs = filter isGovOrg

filterGovOrgs' :: [Client a] -> [Client a]
filterGovOrgs' =
    filter
        ( \case
            GovOrg{} -> True
            _ -> False
        )
