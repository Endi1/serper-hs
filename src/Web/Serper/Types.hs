{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Serper.Types (SerperResult, OrganicGoogleSearchResult, SiteLink, PeopleAlsoAskResult, RelatedSearch) where

import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import GHC.Generics (Generic)

data SerperResult = SerperResult
  { organic :: [OrganicGoogleSearchResult],
    peopleAlsoAsk :: [PeopleAlsoAskResult],
    relatedSearches :: [RelatedSearch]
  }
  deriving (Generic, Show)

instance FromJSON SerperResult

data OrganicGoogleSearchResult = OrganicGoogleSearchResult
  { organicGoogleSearchResultTitle :: String,
    organicGoogleSearchResultLink :: String,
    organicGoogleSearchResultSnippet :: String,
    position :: Int,
    sitelinks :: Maybe [SiteLink],
    date :: Maybe String
  }
  deriving (Show)

instance FromJSON OrganicGoogleSearchResult where
  parseJSON (Object v) =
    OrganicGoogleSearchResult
      <$> v
        .: "title"
      <*> v
        .: "link"
      <*> v
        .: "snippet"
      <*> v
        .: "position"
      <*> v
        .:? "sitelinks"
      <*> v
        .:? "date"
  parseJSON invalid = prependFailure "parsing OrganicGoogleSearchResult failed, " (typeMismatch "Object" invalid)

data SiteLink = SiteLink
  { siteLinkTitle :: String,
    siteLinkLink :: String
  }
  deriving (Show)

instance FromJSON SiteLink where
  parseJSON (Object v) =
    SiteLink
      <$> v
        .: "title"
      <*> v
        .: "link"
  parseJSON invalid = prependFailure "parsing SiteLink failed, " (typeMismatch "Object" invalid)

data PeopleAlsoAskResult = PeopleAlsoAskResult
  { question :: String,
    peopleAlsoAskResultSnippet :: String,
    peopleAlsoAskResultTitle :: String,
    peopleAlsoAskResultLink :: String
  }
  deriving (Show)

instance FromJSON PeopleAlsoAskResult where
  parseJSON (Object v) =
    PeopleAlsoAskResult
      <$> v
        .: "question"
      <*> v
        .: "snippet"
      <*> v
        .: "title"
      <*> v
        .: "link"
  parseJSON invalid = prependFailure "parsing PeopleAlsoAskResult failed, " (typeMismatch "Object" invalid)

newtype RelatedSearch = RelatedSearch
  { query :: String
  }
  deriving (Generic, Show)

instance FromJSON RelatedSearch
