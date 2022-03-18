module Chapter4.LibSpec (spec) where

import Test.Hspec
import Chapter4.Lib
import Data.Tree

spec :: Spec
spec = do
    -- pictureTree :: Tree Int
    -- to silence warning: [-Wtype-defaults]
    let pictureTree = Node (1::Integer) [ Node 2 [ Node 3 [], Node 4 [], Node 5 [], Node 6 []] ]

    let treePreOrder = ["1", "2", "3", "4", "5", "6"]::[[Char]]

    describe "preOrder" $ do
        it "should correctly traverse pictureTree in pre-order" $ do
            preOrder show pictureTree `shouldBe` treePreOrder
            preOrder' show pictureTree `shouldBe` treePreOrder

    describe "Priceable instance for TimeMachine" $ do
        it "should price the TimeMachine correctly" $ do
            let oldTimeMachine = OldTimeMachine "name" (3.0::Double) "StringMachine"
            let newTimeMachine = NewTimeMachine "machine at prom price" (2.0::Double)
            totalPrice oldTimeMachine `shouldBe` 3.0
            totalPrice newTimeMachine `shouldBe` 12.0

    describe "Priceable instance for Cliend" $ do
        it "should equate correctly clients" $ do
            let ind1 = OneIndividual (5::Integer) (Perzon "Doctor" "")
            let ind2 = OneIndividual (6::Integer) (Perzon "Sarah" "Jane")
            let ind3 = OneIndividual (6::Integer) (Perzon "Sarah" "Jane")
            ind1 == ind2 `shouldBe` False
            ind2 == ind3 `shouldBe` True
            let cmp1 = OneCompany (4::Integer) "Wormhole Inc." 1 (Perzon "Karl" "Schwarzschild") "Physicist"
            let cmp2 = OneCompany (4::Integer) "Wormh$le Inc." 1 (Perzon "Karl" "Schwarzschild") "Physicist"
            cmp1 == cmp2 `shouldBe` False

