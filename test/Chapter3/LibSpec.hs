module Chapter3.LibSpec (spec) where

import Test.Hspec
import Chapter3.Lib

spec :: Spec
spec = do
    describe "doubleMapf" $ do
        it "can map with two elements" $ do
            let lst = [(1, 2), (3, 4), (5, 6)]::[(Integer, Integer)]
            doubleMapf (+) lst `shouldBe` [3, 7, 11]
    describe "filterOnes" $ do
        it "can filter basic lists" $ do
            let lst = [1, 1, 2, 3]::[Integer]
            filterOnes lst `shouldBe` [1, 1]
    describe "filterANumber" $ do
        it "can filter with whatever number" $ do
            let lst = [1, 3, 5, 4, 5]::[Integer]
            filterANumber 5 lst `shouldBe` [5, 5]
    describe "filterGovOrgs" $ do
        it "can filter GovOrgs" $ do
            let fstGovOrg = GovOrg 3 "Russia"
            let sndGovOrg = GovOrg 4 "Italy"
            let nonGovOrg = Individual 2 $ Person "a" "b"
            let lst = [fstGovOrg, sndGovOrg, nonGovOrg]::[Client Integer]
            filterGovOrgs lst `shouldBe` [fstGovOrg, sndGovOrg]
    describe "foldr'" $ do
        let lst = [1, 2, 3]::[Integer]
        it "can fold to right" $ do
            foldr' (+) 0 lst `shouldBe` 6
        it "can fold `InfNumber`s with normal numbers" $ do
            foldr' infMax MinusInf (map Number lst) `shouldBe` (Number 3)