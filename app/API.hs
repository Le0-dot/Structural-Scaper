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
import Data.Text (Text)
import Control.Monad (forM)


data Page = Index | New | Edit Draft [(ExtractorDraft, Text)]

instance ToMarkup Page where
    toMarkup Index = index
    toMarkup New = new
    toMarkup (Edit d es) = undefined

type API = Get '[HTML] Page
        :<|> "new" :> Get '[HTML] Page
        :<|> "edit" :> Capture "id" Int32 :> Get '[HTML] Page
        :<|> "static" :> Raw

server :: Connection -> Server API
server conn = do
    return Index
    :<|> return New
    :<|> (editController conn)
    :<|> serveDirectoryWebApp "static/src"

editController :: Connection -> Int32 -> Handler Page
editController conn draftId = do
    draft <- liftIO $ runSelectOne conn $ getDraftForId draftId
    case draft of
        Nothing -> throwError err404 { errBody = "Could not find draft with such id" }
        Just d -> do
            extractors <- liftIO $ runSelectList conn $ getExtractorsForDraft d
            selectors <- forM extractors $ liftIO . makeSelector conn
            return $ Edit d $ zip extractors selectors

makeSelector :: Connection -> ExtractorDraft -> IO Text
makeSelector conn extractor = do
    selectors <- runSelectList conn $ getSelectors extractor
    selectorClasses <- forM selectors $ runSelectList conn . getSelectorClasses
    return $ buildSelector selectors selectorClasses

apiProxy :: Proxy API
apiProxy = Proxy

app :: Connection -> Application
app conn = serve apiProxy $ server conn
