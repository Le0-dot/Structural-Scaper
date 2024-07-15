{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Data.Text (Text)
import Util
import Models
import Data.Aeson


data EditExtractorRequest = EditExtractorRequest Text (StringMaybe ExtractorType)

instance FromJSON EditExtractorRequest where
    parseJSON = withObject "EditExtractorRequest" $ \v -> EditExtractorRequest
        <$> v .: "name"
        <*> v .: "value"


data NewFilenameTemplateRequest = NewFilenameTemplateRequest Text

instance FromJSON NewFilenameTemplateRequest where
    parseJSON = withObject "NewFilenameTemplateRequest" $ \v -> NewFilenameTemplateRequest
        <$> v .: "filename"


data NewNextTemplateRequest = NewNextTemplateRequest Text

instance FromJSON NewNextTemplateRequest where
    parseJSON = withObject "NewNextTemplateRequest" $ \v -> NewNextTemplateRequest
        <$> v .: "next"


data NewContentTemplateRequest = NewContentTemplateRequest Text

instance FromJSON NewContentTemplateRequest where
    parseJSON = withObject "NewContentTemplateRequest" $ \v -> NewContentTemplateRequest
        <$> v .: "content"
