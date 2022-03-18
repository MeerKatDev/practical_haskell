module Chapter5.LibSpec (spec) where

import Test.Hspec
import Chapter5.Lib
import Data.List


spec :: Spec
spec = do

    describe "timeMachinesFrom" $ do
        it "should give three equal elements with take" $ do
            let result = fmap (\n -> TM "Timely Inc." n) [100, 101, 102]
            take 3 timelyIncMachines `shouldBe` result
        it "should contain the first which travels after 2018" $ do
            let result = Just $ TM {manufacturer = "Timely Inc.", year = 2019}
            find (\(TM {year = y}) -> y > 2018) timelyIncMachines `shouldBe` result


