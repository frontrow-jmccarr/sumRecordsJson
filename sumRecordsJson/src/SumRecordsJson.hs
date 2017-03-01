{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SumRecordsJson where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types (Options(..))
import Cases

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
  deriving Generic

data GradeDomain
  = GradeDomain
    { _gradeDomainGrade :: Grade
    , _gradeDomainDomain :: Domain
    }
  deriving Generic

data GradeDomainStandard
  = GradeDomainStandard
    { _gradeDomainStandardGrade :: Grade
    , _gradeDomainStandardDomain :: Domain
    , _gradeDomainStandardStandard :: Standard
    }
  deriving Generic

data SumRecordsJson
  = GA GradeAlone
  | GD GradeDomain
  | GS GradeDomainStandard
  deriving Generic

instance FromJSON GradeAlone where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeAlone"

instance FromJSON GradeDomain where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomain"

instance FromJSON GradeDomainStandard where
  parseJSON = genericParseJSON $ genericDropOptions "_gradeDomainStanard"

instance FromJSON SumRecordsJson where
  parseJSON = genericParseJSON  defaultOptions
