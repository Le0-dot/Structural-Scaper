{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import GHC.Generics
import Data.Int
import Control.Monad.IO.Class (liftIO)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (ToMarkup(toMarkup))
import Database.SQLite.Simple
import View
import Models
import CRUD
import Data.Text (Text)
import Control.Monad (forM)
import Data.Aeson


data Index = Index
data New = New
data Edit = Edit Draft [ExtractorDraft] [Text]
data EditExtractorResponse = EditExtractorResponse ExtractorDraft Text

instance ToMarkup Index where
    toMarkup _ = index

instance ToMarkup New where
    toMarkup _ = new

instance ToMarkup Edit where
    toMarkup (Edit d es ts) = draftView d es ts

instance ToMarkup EditExtractorResponse where
    toMarkup (EditExtractorResponse extractor selector) = extractorView extractor selector


newtype StringMaybe a = StringMaybe { stringMaybe :: Maybe a }

instance FromJSON a => FromJSON (StringMaybe a) where
    parseJSON "" = pure $ StringMaybe Nothing
    parseJSON x = StringMaybe <$> parseJSON x

data EditExtractorRequest = EditExtractorRequest Text (StringMaybe ExtractorType)
    deriving (Generic)

instance FromJSON EditExtractorRequest where
    parseJSON = withObject "EditExtractorRequest" $ \v -> EditExtractorRequest
        <$> v .: "name"
        <*> v .: "value"

type API = Get '[HTML] Index
        :<|> "new" :> Get '[HTML] New
        :<|> "edit" :> Capture "id" Int32 :> Get '[HTML] Edit
        :<|> "edit" :> "extractor" :> Capture "id" Int32 :> ReqBody '[JSON] EditExtractorRequest :> Put '[HTML] EditExtractorResponse
        :<|> "static" :> Raw


server :: Connection -> Server API
server conn = do
    return Index
    :<|> return New
    :<|> (editController conn)
    :<|> (editExtractorController conn)
    :<|> serveDirectoryWebApp "static/src"

editController :: Connection -> Int32 -> Handler Edit
editController conn draftId = do
    draft <- liftIO $ runSelectOne conn $ getDraftForId draftId
    case draft of
        Nothing -> throwError err404 { errBody = "Could not find draft with such id" }
        Just d -> do
            extractors <- liftIO $ runSelectList conn $ getExtractorsForDraft d
            selectors <- forM extractors $ liftIO . makeSelector conn
            return $ Edit d extractors selectors

makeSelector :: Connection -> ExtractorDraft -> IO Text
makeSelector conn extractor = do
    selectors <- runSelectList conn $ getSelectors extractor
    selectorClasses <- forM selectors $ runSelectList conn . getSelectorClasses
    return $ buildSelector selectors selectorClasses

editExtractorController :: Connection -> Int32 -> EditExtractorRequest -> Handler EditExtractorResponse
editExtractorController conn exId (EditExtractorRequest newName newType) = do
    extractor <- liftIO $ do
        runUpdate conn $ updateExtractorDraft exId newName $ stringMaybe newType
        runSelectOne conn $ getExtractorDraft exId
    case extractor of
        Nothing -> throwError err404 { errBody = "Could not find extractor with such id" }
        Just ex -> do
            selector <- liftIO $ makeSelector conn ex
            return $ EditExtractorResponse ex selector

apiProxy :: Proxy API
apiProxy = Proxy

app :: Connection -> Application
app = serve apiProxy . server
