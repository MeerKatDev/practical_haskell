{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Chapter2.Lib where

data Gender = Male | Female | Dunno
    deriving (Show)

data Person
    = PersonNoGender String String
    | Person String String Gender
    deriving (Show)

data Client
    = GovOrg String
    | Company String Integer String String
    | Individual Person Bool
    deriving (Show)

clientName :: Client -> String
clientName (GovOrg name) = name
clientName (Company name _ _ _) = name
clientName (Individual (Person firstName lastName _) _) = firstName ++ " " ++ lastName

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n -1) + fibonacci (n -2)

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = case lst1 of
    [] -> lst2
    x : xs -> x : (xs +++ lst2)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : zs) = x < y && sorted (y : zs)

maxmin :: Ord x => [x] -> (x, x)
maxmin [x] = (x, x)
maxmin (x : xs) = (choose_min x, choose_max x)
  where
    (xs_min, xs_max) = maxmin xs
    choose_max x = if x > xs_max then x else xs_max
    choose_min x = if x < xs_min then x else xs_min

ifib :: Integer -> Maybe Integer
ifib n | n < 0 = Nothing
ifib 0 = Just 0
ifib 1 = Just 1
ifib n
    | otherwise =
        let Just f1 = ifib (n -1)
            Just f2 = ifib (n -2)
         in Just (f1 + f2)

binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom n k | n == k = 1 -- unnecessary but ok
binom n k = (binom (n -1) (k -1)) + (binom (n -1) k)

ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n + 1
ackermann m 0 | m > 0 = ackermann (m -1) 1
ackermann m n | m > 0 && n > 0 = ackermann (m -1) (ackermann m (n -1))

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise = show n ++ " is a beautiful number"

unzip' :: [(Integer, Integer)] -> ([Integer], [Integer])
unzip' [] = ([], [])
unzip' ((a, b) : xs) = (a : fst rest, b : snd rest)
  where
    rest = unzip' xs

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

-- view patterns
-- new syntax element: (function -> pattern)

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True -- this -> is a view pattern
specialClient (responsibility -> "Director") = True
specialClient _ = False

-- records

data PersonR = PersonR {firstName :: String, lastName :: String} deriving (Show)

-- same attributes in two different constructors must have the same type
data ClientR
    = GovOrgR {clientRName :: String}
    | CompanyR
        { clientRName :: String
        , companyId :: Integer
        , person :: PersonR
        , duty :: String
        }
    | IndividualR {person :: PersonR}
    deriving (Show)

greet'' :: ClientR -> String
greet'' IndividualR{person = PersonR{firstName = fn}} = "Hi, " ++ fn
greet'' CompanyR{clientRName = c} = "Hi, " ++ c
greet'' GovOrgR{} = "Welcome"

-- with NamedFieldPuns:
greet' :: ClientR -> String
greet' IndividualR{person = PersonR{firstName}} = "Hi, " ++ firstName
greet' CompanyR{clientRName} = "Hi, " ++ clientRName
greet' GovOrgR{} = "Welcome"

-- with RecordWildCards
greet :: ClientR -> String
greet IndividualR{person = PersonR{..}} = "Hi, " ++ firstName
greet CompanyR{..} = "Hi, " ++ clientRName
greet GovOrgR{} = "Welcome"

