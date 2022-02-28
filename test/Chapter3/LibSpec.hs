module Chapter3.LibSpec (spec) where

import Test.Hspec
import Chapter3.Lib

spec :: Spec
spec = do
    describe "doubleMapf" $ do
        it "can map with two elements" $ do
            doubleMapf (+) [(1, 2), (3, 4), (5, 6)] `shouldBe` [3, 7, 11]
    describe "filterOnes" $ do
        it "can filter basic lists" $ do
            filterOnes [1, 1, 2, 3] `shouldBe` [1, 1]
    describe "filterANumber" $ do
        it "can filter with whatever number" $ do
            filterANumber 5 [1, 3, 5, 4, 5] `shouldBe` [5, 5]
    describe "filterGovOrgs" $ do
        it "can filter GovOrgs" $ do
            let fstGovOrg = GovOrg 3 "Russia"
            let sndGovOrg = GovOrg 4 "Italy"
            let nonGovOrg = Individual 2 $ Person "a" "b"
            let lst = [fstGovOrg, sndGovOrg, nonGovOrg]
            filterGovOrgs lst `shouldBe` [fstGovOrg, sndGovOrg]
    describe "foldr'" $ do
        it "can fold to right" $ do
            foldr' (+) 0 [1, 2, 3] `shouldBe` 6
        -- it "can fold `InfNumber`s with normal numbers" $ do
        --     foldr' infMax MinusInf (map Number [1, 2, 3])