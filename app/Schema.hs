{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Data.Text (Text)
import Util
import Models
import GHC.Generics (Generic)
import Data.Aeson


data EditExtractorRequest = EditExtractorRequest Text (StringMaybe ExtractorType)
    deriving (Generic)

instance FromJSON EditExtractorRequest where
    parseJSON = withObject "EditExtractorRequest" $ \v -> EditExtractorRequest
        <$> v .: "name"
        <*> v .: "value"
