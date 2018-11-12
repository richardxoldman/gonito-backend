{-# LANGUAGE OverloadedStrings #-}

module Gonito.ExtractMetadataSpec (spec) where

import Import

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Test.Hspec
import Gonito.ExtractMetadata (extractMetadataFromRepoDir, GonitoMetadata(..), ExtractionOptions(..), Link(..))

spec :: Spec
spec = do
  describe "extract metadata from repos" $ do
    it "simple" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/simple/" def `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Simple solution",
        gonitoMetadataTags = S.fromList ["foo", "simple-solution", "baz"],
        gonitoMetadataGeneralParams = M.empty,
        gonitoMetadataExternalLinks = []
        }
    it "simple with some fields from the form" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/simple/" def {
        extractionOptionsDescription = Just "Other solution",
        extractionOptionsTags = Just $ S.fromList ["other-tag", "baz"]
        } `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Other solution",
        gonitoMetadataTags = S.fromList ["foo", "simple-solution", "baz", "other-tag"],
        gonitoMetadataGeneralParams = M.empty,
        gonitoMetadataExternalLinks = []
        }
    it "with gonito.yaml" $ do
      extractMetadataFromRepoDir "test/fake-git-repos/with-gonito-yaml/" def `shouldReturn` GonitoMetadata {
        gonitoMetadataDescription = "Test solution",
        gonitoMetadataTags = S.fromList ["zzz", "baz", "simple", "machine-learning"],
        gonitoMetadataGeneralParams = M.fromList [("level", "4"),
                                                  ("altitude", "8900.3"),
                                                  ("q", "10.4"),
                                                  ("style", "bold")],
        gonitoMetadataExternalLinks = [
            Link (Just "gitlab") "https://about.gitlab.com/",
            Link (Just "Polish Wikipedia") "https://pl.wikipedia.org/wiki/Wikipedia:Strona_g%C5%82%C3%B3wna",
            Link Nothing "https://tvtropes.org/" ]
        }
