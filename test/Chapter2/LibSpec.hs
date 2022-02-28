module Chapter2.LibSpec (spec) where

import Test.Hspec
import Chapter2.Lib

spec :: Spec
spec = do
    describe "sorted" $ do
        it "checks if a list is sorted" $ do
            sorted [1, 2, 3, 4] `shouldBe` True
            sorted [2, 1, 3, 4] `shouldBe` False