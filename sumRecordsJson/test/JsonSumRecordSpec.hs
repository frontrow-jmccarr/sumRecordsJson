{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module JsonSumRecordSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Aeson
import Data.Aeson.QQ
import MathAssessmentMetadata

main :: IO ()
main = hspec spec

justGrade :: Value
justGrade = [aesonQQ| {"grade": 3} |]

spec :: Spec
spec = do
  describe "parseSumRecord" $ do
    it "parse just a grade" $ do
      let ga = GA $ GradeAlone 3
      decode (encode ga) `shouldBe` Just ga
    it "parse grade and domain" $ do
      let gd = GD $ GradeDomain 3 "iphiukap"
      decode (encode gd) `shouldBe` Just gd
    it "parse grade, domain and standard" $ do
      let gs = GS $ GradeDomainStandard 3 "iphiukap" "quiobauw"
      decode (encode gs) `shouldBe` Just gs
