{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module SumRecordsJson where

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

data GradeAlone
  = GradeAlone
    { _gradeAloneGrade :: Grade
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeAlone where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeAlone"

instance ToJSON GradeAlone where
  toJSON GradeAlone{..} =
    object [ "grade" .= _gradeAloneGrade ]


data GradeDomain
  = GradeDomain
    { _gradeDomainGrade :: Grade
    , _gradeDomainDomain :: Domain
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeDomain where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomain"

instance ToJSON GradeDomain where
  toJSON GradeDomain{..} =
    object [
        "grade" .= _gradeDomainGrade
      , "domain" .= _gradeDomainDomain ]


data GradeDomainStandard
  = GradeDomainStandard
    { _gradeDomainStandardGrade :: Grade
    , _gradeDomainStandardDomain :: Domain
    , _gradeDomainStandardStandard :: Standard
    }
  deriving (Eq, Show, Generic)

instance FromJSON GradeDomainStandard where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomainStandard"

instance ToJSON GradeDomainStandard where
  toJSON GradeDomainStandard{..} =
    object [
        "grade" .= _gradeDomainStandardGrade
      , "domain" .= _gradeDomainStandardDomain
      , "standard" .= _gradeDomainStandardStandard ]


data SumRecordsJson
  = GA GradeAlone
  | GD GradeDomain
  | GS GradeDomainStandard
  deriving (Eq, Show, Generic)

instance FromJSON SumRecordsJson where
  parseJSON = withObject "MathAssessment metadata" $ \o ->
    -- must be ordered from most specific to least specific parsers
    GS <$> parseJSON (Object o)
    <|> GD <$> parseJSON (Object o)
    <|> GA <$> parseJSON (Object o)
    -- <?> fail "could not parse"

instance ToJSON SumRecordsJson where
  toJSON g = case g of
    GA gradeAlone -> toJSON gradeAlone
    GD gradeDomain -> toJSON gradeDomain
    GS gradeDomainStandard -> toJSON gradeDomainStandard
