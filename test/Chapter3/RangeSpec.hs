module Chapter3.RangeSpec (spec) where

import Test.Hspec
import Chapter3.Range

spec :: Spec
spec = do
    describe "prettyRange" $ do
        it "pattern matches correctly and pretty prints the range" $ do
            prettyRange (range 2 4) `shouldBe` "[2, 4]"