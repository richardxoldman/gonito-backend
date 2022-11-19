{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SubmissionConditionsSpec (spec) where

import Test.Hspec
import Import

import Database.Persist.Sql (toSqlKey)

import Data.SubmissionConditions

spec :: Spec
spec = do
  describe "simple conditions" $ do
    it "just param" $ do
      parseCondition "nb-foo" `shouldBe` (Just $ Simple $ Existence $ ValueHolder "nb-foo")
    it "with operators" $ do
      parseCondition "baz<8.5" `shouldBe` (Just $ Simple $ SBinary (ReadFrom $ ValueHolder "baz")
                                                                   Less
                                                                   (DoubleAtom 8.5))
      parseCondition "baz<=10" `shouldBe` (Just $ Simple $ SBinary (ReadFrom $ ValueHolder "baz")
                                                                   LessOrEqual
                                                                   (IntegerAtom 10))
    it "with spaces" $ do
      parseCondition "foo > 3.2" `shouldBe` (Just $ Simple $ SBinary (ReadFrom $ ValueHolder "foo")
                                                                     Greater
                                                                     (DoubleAtom 3.2))
      parseCondition "foo = -3" `shouldBe` (Just $ Simple $ SBinary (ReadFrom $ ValueHolder "foo")
                                                                     Equal
                                                                    (IntegerAtom (-3)))
      parseCondition " baz ==   ' a b c'   " `shouldBe` (Just $ Simple $ SBinary (ReadFrom $ ValueHolder "baz")
                                                                      Equal
                                                                      (StringAtom " a b c"))
    it "boolean conditions" $ do
      parseCondition "!nb-epochs && x <> 5|| y='a'" `shouldBe` (
        Just $ BBinary Or (BBinary And (Neg (Simple $ Existence $ ValueHolder "nb-epochs"))
                                       (Simple $ SBinary (ReadFrom $ ValueHolder "x")
                                                         NotEqual
                                                         (IntegerAtom 5)))
                          (Simple $ SBinary (ReadFrom $ ValueHolder "y")
                                            Equal
                                            (StringAtom "a")))
    it "with parens" $ do
      parseCondition "foo && (x != 10 || !y)" `shouldBe` (
        Just $ BBinary And (Simple $ Existence $ ValueHolder "foo")
                           (BBinary Or (Simple $ SBinary (ReadFrom $ ValueHolder "x")
                                                         NotEqual
                                                         (IntegerAtom 10))
                                       (Neg $ Simple $ Existence $ ValueHolder "y")))
  describe "running conditions" $ do
    it "simple condition" $ do
      let fakeKey :: Key Variant = toSqlKey 1
      let sampleEntry = VariantEntry {
            variantEntryTags = [Tag "foo" Nothing Nothing, Tag "neural-network" (Just "description") Nothing],
            variantEntryParams = [Parameter fakeKey "z" "80", Parameter fakeKey "learning-rate" "0.0001", Parameter fakeKey "type" "supervised"]
            }
      checkCondition (parseCondition "z < 100") sampleEntry `shouldBe` True
      checkCondition (parseCondition "other <= 3.14") sampleEntry `shouldBe` False
      checkCondition (parseCondition "other > 3.14") sampleEntry `shouldBe` False
      checkCondition (parseCondition "learning-rate < 0.03 && (type == 'unsupervised' || foo) && !stupid") sampleEntry `shouldBe` True
      checkCondition (parseCondition "z<>80||!foo") sampleEntry `shouldBe` False
