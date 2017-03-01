module JsonSumRecordSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Aeson
import AesonQQ
import SumRecordsJson

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSumRecord" $ do
    it "correctly parses a sum of records from JSON" $ do
      parseSumRecord [aesonQQ| {"age": 23, "foo": "bar"}| ] `shouldBe` 0
