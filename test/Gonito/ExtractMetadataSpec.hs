{-# LANGUAGE OverloadedStrings #-}

module Gonito.ExtractMetadataSpec (spec) where

import Import

import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

import Test.Hspec
import Gonito.ExtractMetadata (extractMetadataFromRepoDir, GonitoMetadata(..), ExtractionOptions(..))

spec :: Spec
spec = do
  describe "extract metadata from repos" $ do
    it "simple" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/simple/" def `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Simple solution",
        gonitoMetadataTags = S.fromList ["foo", "simple-solution", "baz"],
        gonitoMetadataGeneralParams = H.empty
        }
    it "simple with some fields from the form" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/simple/" def {
        extractionOptionsDescription = Just "Other solution",
        extractionOptionsTags = Just "other-tag,baz"
        } `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Other solution",
        gonitoMetadataTags = S.fromList ["foo", "simple-solution", "baz", "other-tag"],
        gonitoMetadataGeneralParams = H.empty
        }
    it "with gonito.yaml" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/with-gonito-yaml/" def `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Test solution",
        gonitoMetadataTags = S.fromList ["zzz", "baz", "simple", "machine-learning"],
        gonitoMetadataGeneralParams = H.empty
        }
