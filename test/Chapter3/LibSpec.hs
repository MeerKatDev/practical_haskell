module Chapter3.LibSpec (spec) where

import Test.Hspec
import Chapter3.Lib
import Control.Exception (evaluate)
import Prelude hiding (foldr, foldl, product, all)

spec :: Spec
spec = do
    -- clients list fixture
    let basicList = [1, 2, 3]::[Integer]
    let fstGovOrg = GovOrg 3 "Russia"
    let sndGovOrg = GovOrg 4 "Italy"
    let nonGovOrg = Individual 2 $ Person "a" "b"
    let clientsList = [fstGovOrg, sndGovOrg, nonGovOrg]::[Client Integer]

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
            filterGovOrgs clientsList `shouldBe` [fstGovOrg, sndGovOrg]

    describe "foldr'" $ do
        it "folds to right summing" $ do
            foldr (+) 0 basicList `shouldBe` 6
        it "folds to right correctly" $ do
            foldr (-) 0 basicList `shouldBe` 2
        it "can fold `InfNumber`s with normal numbers" $ do
            foldr infMax MinusInf (map Number basicList) `shouldBe` (Number 3)

    describe "foldl'" $ do
        it "folds to left correctly" $ do
            foldl (-) 0 basicList `shouldBe` -6

    describe "product'" $ do
        it "computes product of a list" $ do
            let lst = [1, 2, 3]::[Integer]
            product lst `shouldBe` 6

    describe "minimumClient" $ do
        it "gets the client with the smallest clientName" $ do
            minimumClient clientsList `shouldBe` nonGovOrg
        it "throws an error for an empty list" $ do
            evaluate( minimumClient [] ) `shouldThrow` errorCall "no clients in this list!"

    describe "minimumBy" $ do
        it "gives the minimum by first using a transform" $ do
            minimumBy (\x -> -x) basicList `shouldBe` 3


