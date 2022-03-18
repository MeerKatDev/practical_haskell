{-# LANGUAGE LambdaCase #-}

module Chapter4.Lib where
    import qualified Data.Map  as M
    import qualified Data.Set  as S
    import Data.Tree
    import Chapter3.Lib(Client(..), Person(..))

    -- Exercise 4.3: Classifying clients
    data ClientKind = GovOrgKind | CompanyKind | IndividualKind

    -- emptyClassifier :: M.Map ClientKind (S.Set(Client Integer))
    -- emptyClassifier = M.fromList [(GovOrgKind, S.fromList []), (CompanyKind, S.fromList []), (IndividualKind, S.fromList [])]

    -- classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set(Client Integer))
    -- classifyClients lst = rec lst emptyClassifier
    --  where
    --      rec (x:xs) mapDeposit = case x of
    --          GovOrg{}     -> M.alter (\x -> S.insert x) GovOrgKind mapDeposit
    --          Company{}    -> M.alter (\x -> S.insert x) CompanyKind mapDeposit
    --          Individual{} -> M.alter (\x -> S.insert x) IndividualKind mapDeposit

    -- acts on each Tree element and applies a function on it
    -- it goes in pre-order
    -- breadth-first -> fun levels
    -- pre-order -> flatten
    preOrder :: (a -> b) -> Tree a -> [b]
    preOrder f (Node v subtrees) =
        let subtreesTraversed = concat $ map (preOrder f) subtrees
        in f v:subtreesTraversed

    preOrder' :: (a -> b) -> Tree a -> [b]
    preOrder' f (Node v subtrees) =
        let subtreesTraversed = concatMap (preOrder' f) subtrees
        in f v:subtreesTraversed

    -- talking about Graphs now:
    -- ---

    -- type classes!
    -- declaration
    class Nameable var where
        name :: var -> String

    -- instance. Here, there must be the whole type included. the type parameter must be included, if present.
    instance Nameable (Client c) where
        name Individual { person = Person { firstName = fn, lastName = ln} } = fn ++ " " ++ ln
        name c = clientName c
    ---
    -- Exercise 4.4: Prices for the store
    class Priceable p where
        totalPrice :: p -> Double

    data TimeMachine idType =
        OldTimeMachine { tmName :: [Char], price :: Double, typeWorld :: idType }
        | NewTimeMachine { tmName :: [Char], price :: Double } -- the world is chosen automatically in this new version!
         deriving (Eq, Show)

    instance Priceable (TimeMachine tm) where
        totalPrice NewTimeMachine { price = p } = p + 10.0
        totalPrice OldTimeMachine { price = p } = p

    -- Exercise 4.5: The same client
    -- needed to rederive Eq, so I needed to copy them over
    data Perzon = Perzon {oname :: String, surname :: String} deriving (Show)

    data Cliend idType
        = OneGovOrg
            { cliendId :: idType
            , cliendName :: String
            }
        | OneCompany
            { cliendId :: idType
            , cliendName :: String
            , companieId :: Integer
            , perzon :: Perzon
            , dutie :: String
            }
        | OneIndividual
            { cliendId :: idType
            , perzon :: Perzon
            }
        deriving (Show)

    instance Eq Perzon where
        Perzon { oname = fn1, surname = ln1 } == Perzon { oname = fn2, surname = ln2 }
            = fn1 == fn2 && ln1 == ln2

    instance Eq c => Eq (Cliend c) where
        OneIndividual { cliendId = cid1, perzon = p1 } == OneIndividual { cliendId = cid2, perzon = p2 }
            = cid1 == cid2 && p1 == p2
        OneCompany { cliendId = cid1, cliendName = cn1, companieId = coid1, perzon = p1, dutie = d1 }
            == OneCompany { cliendId = cid2, cliendName = cn2, companieId = coid2, perzon = p2, dutie = d2 }
                = cid1 == cid2 && cn1 == cn2 && coid1 == coid2 && p1 == p2 && d1 == d2
        OneGovOrg { cliendId = cid1, cliendName = cn1 } == OneGovOrg { cliendId = cid2, cliendName = cn2 }
            = cid1 == cid2 && cn1 == cn2
        _ == _ = False
