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


data Index = Index
data New = New
data Edit = Edit Draft [ExtractorDraft] [Text]

instance ToMarkup Index where
    toMarkup _ = index

instance ToMarkup New where
    toMarkup _ = new

instance ToMarkup Edit where
    toMarkup (Edit d es ts) = draftView d es ts

type API = Get '[HTML] Index
        :<|> "new" :> Get '[HTML] New
        :<|> "edit" :> Capture "id" Int32 :> Get '[HTML] Edit
        :<|> "static" :> Raw

server :: Connection -> Server API
server conn = do
    return Index
    :<|> return New
    :<|> (editController conn)
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

apiProxy :: Proxy API
apiProxy = Proxy

app :: Connection -> Application
app = serve apiProxy . server
