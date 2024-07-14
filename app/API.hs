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

import Data.Int
import Control.Monad.IO.Class (liftIO)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (ToMarkup(toMarkup))
import Database.SQLite.Simple
import View
import Models
import CRUD
import Control.Monad (forM)
import Schema
import Util


data Index = Index
data New = New
data Edit = Edit Draft [ExtractorDraft]
data EditExtractorResponse = EditExtractorResponse ExtractorDraft

instance ToMarkup Index where
    toMarkup _ = index

instance ToMarkup New where
    toMarkup _ = new

instance ToMarkup Edit where
    toMarkup (Edit d es) = draftView d es

instance ToMarkup EditExtractorResponse where
    toMarkup (EditExtractorResponse extractor) = extractorView extractor


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
            extractorsWithSelectors <- forM extractors $ liftIO . updateSelectorCache conn
            return $ Edit d extractorsWithSelectors


updateSelectorCache :: Connection -> ExtractorDraft -> IO ExtractorDraft
updateSelectorCache conn extractor = do
    selectors <- runSelectList conn $ getSelectors extractor
    selectorClasses <- forM selectors $ runSelectList conn . getSelectorClasses
    let selector = buildSelector selectors selectorClasses
    let newExtractor = extractor { _extractorDraftSelectorCache = selector }
    runAndReturn (runUpdate conn . saveExtractorDraft) newExtractor


editExtractorController :: Connection -> Int32 -> EditExtractorRequest -> Handler EditExtractorResponse
editExtractorController conn exId (EditExtractorRequest newName newType) = do
    extractor <- liftIO $ runSelectOne conn $ getExtractorDraft exId
    case extractor of
        Nothing -> throwError err404 { errBody = "Could not find extractor with such id" }
        Just ex -> do
            let newExtractor = ex { _extractorDraftName = newName, _extractorDraftType = stringMaybe newType }
            liftIO $ runUpdate conn $ saveExtractorDraft newExtractor
            return $ EditExtractorResponse newExtractor

apiProxy :: Proxy API
apiProxy = Proxy

app :: Connection -> Application
app = serve apiProxy . server
