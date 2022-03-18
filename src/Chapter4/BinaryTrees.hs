module Chapter4.BinaryTrees where

    data TravelGuide = TravelGuide { title :: String, authors :: [String], price :: Double } deriving (Show, Eq, Ord)

    -- Step 1: A simple binary tree.
    data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1
                     | Leaf1
                     deriving (Show)

    treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
    treeFind1 t (Node1 v l r) = case compare t v of
                                    EQ -> Just v
                                    LT -> treeFind1 t l
                                    GT -> treeFind1 t r
    treeFind1 _ Leaf1         = Nothing

    treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
    treeInsert1 t n@(Node1 v l r) = case compare t v of
                                        EQ -> n
                                        LT -> Node1 v (treeInsert1 t l) r
                                        GT -> Node1 v l (treeInsert1 t r)
    treeInsert1 t Leaf1           = Node1 t Leaf1 Leaf1

    -- Step 2: Polymorphic Binary Trees
    data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                       | Leaf2
                       deriving (Show)

    treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
    treeFind2 t (Node2 v l r) = case compare t v of
                                    EQ -> Just v
                                    LT -> treeFind2 t l
                                    GT -> treeFind2 t r
    treeFind2 _ Leaf2         = Nothing

    treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
    treeInsert2 t n@(Node2 v l r) = case compare t v of
                                        EQ -> n
                                        LT -> Node2 v (treeInsert2 t l) r
                                        GT -> Node2 v l (treeInsert2 t r)
    treeInsert2 t Leaf2           = Node2 t Leaf2 Leaf2


    -- we want the Ord to compare by price first, not lexycographically by title
    newtype TGByPrice = TGByPrice TravelGuide deriving (Eq)

    instance Ord TGByPrice where
        (TGByPrice (TravelGuide t1 a1 p1)) <= (TGByPrice (TravelGuide t2 a2 p2)) =
            p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

    -- Step 3: Binary Trees with Monoidal Cache
    -- We cache the price of the smallest element, per element
    data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                         | Leaf3
                         deriving (Show, Eq, Ord)

    treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
    treeInsert3 v c (Node3 v2 c2 l r) = case compare v v2 of
                                        EQ -> Node3 v2 c2 l r
                                        LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
                                        GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
    treeInsert3 v c Leaf3           = Node3 v c Leaf3 Leaf3


    treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
    treeInsert4 v c (Node3 v2 c2 l r) = case compare v v2 of
        EQ -> Node3 v2 c2 l r
        LT -> let newLeft = treeInsert4 v c l
                  newCache = c2 <> cached newLeft <> cached r
              in Node3 v2 newCache newLeft r
        GT -> let newRight = treeInsert4 v c r
                  newCache = c2 <> cached l <> cached newRight
              in Node3 v2 newCache l newRight
    treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

    cached :: Monoid c => BinaryTree3 v c -> c
    cached (Node3 _ c _ _) = c
    cached Leaf3           = mempty
