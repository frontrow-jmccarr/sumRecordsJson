{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module MathAssessmentMetadata where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types (Options(..))
import Cases
import Data.Foldable (asum)

-- for encode at the repl
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

genericDropOptions :: String -> Options
genericDropOptions word =
  defaultOptions
    { fieldLabelModifier = unpack . snakify . fromString . drop (length word)
    , omitNothingFields  = True
    }

type Grade = Integer
type Domain = String
type Standard = String

data GradeAloneFields
  = GradeAloneFields
    { _gradeAloneGrade :: Grade
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeAloneFields where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeAlone"

instance ToJSON GradeAloneFields where
  toJSON GradeAloneFields{..} =
    object [ "grade" .= _gradeAloneGrade ]


data GradeDomainFields
  = GradeDomainFields
    { _gradeDomainGrade :: Grade
    , _gradeDomainDomain :: Domain
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeDomainFields where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomain"

instance ToJSON GradeDomainFields where
  toJSON GradeDomainFields{..} =
    object [
        "grade" .= _gradeDomainGrade
      , "domain" .= _gradeDomainDomain ]


data GradeDomainStandardFields
  = GradeDomainStandardFields
    { _gradeDomainStandardGrade :: Grade
    , _gradeDomainStandardDomain :: Domain
    , _gradeDomainStandardStandard :: Standard
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeDomainStandardFields where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomainStandard"

instance ToJSON GradeDomainStandardFields where
  toJSON GradeDomainStandardFields{..} =
    object [
        "grade" .= _gradeDomainStandardGrade
      , "domain" .= _gradeDomainStandardDomain
      , "standard" .= _gradeDomainStandardStandard ]


data MathAssessmentMetadata
  = MAMGrade GradeAloneFields
  | MAMGradeDomain GradeDomainFields
  | MAMGradeDomainStandard GradeDomainStandardFields
  deriving (Eq, Show, Generic)

instance FromJSON MathAssessmentMetadata where
  parseJSON = withObject "MathAssessment metadata" $ \o ->
    -- must be ordered from most specific to least specific parsers
    MAMGradeDomainStandard <$> parseJSON (Object o)
    <|> MAMGradeDomain <$> parseJSON (Object o)
    <|> MAMGrade <$> parseJSON (Object o)
    -- <?> fail "could not parse"

instance ToJSON MathAssessmentMetadata where
  toJSON g = case g of
    MAMGrade gradeAlone -> toJSON gradeAlone
    MAMGradeDomain gradeDomain -> toJSON gradeDomain
    MAMGradeDomainStandard gradeDomainStandard -> toJSON gradeDomainStandard
