module Main where

import Test.Hspec
import qualified Chapter2.Lib as Lib2
import qualified Chapter3.Lib as Lib3

main :: IO ()
main = hspec $ do
    describe "Chapter2.Lib" $ do
        describe "sorted" $ do
            it "checks if a list is sorted" $ do
                Lib2.sorted [1, 2, 3, 4] `shouldBe` True
                Lib2.sorted [2, 1, 3, 4] `shouldBe` False
    describe "Chapter3.Lib" $ do
        describe "doubleMapf" $ do
            it "can map with two elements" $ do
                Lib3.doubleMapf (+) [(1, 2), (3, 4), (5, 6)] `shouldBe` [3, 7, 11]
        describe "filterOnes" $ do
            it "can filter basic lists" $ do
                Lib3.filterOnes [1, 1, 2, 3] `shouldBe` [1, 1]
        describe "filterANumber" $ do
            it "can filter with whatever number" $ do
                Lib3.filterANumber 5 [1, 3, 5, 4, 5] `shouldBe` [5, 5]
        describe "filterGovOrgs" $ do
            it "can filter GovOrgs" $ do
                let fstGovOrg = Lib3.GovOrg 3 "Russia"
                let sndGovOrg = Lib3.GovOrg 4 "Italy"
                let nonGovOrg = Lib3.Individual 2 $ Lib3.Person "a" "b"
                let lst = [fstGovOrg, sndGovOrg, nonGovOrg]
                Lib3.filterGovOrgs lst `shouldBe` [fstGovOrg, sndGovOrg]
