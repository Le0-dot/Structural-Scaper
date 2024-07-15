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
import Data.Text (Text)


data Index = Index
data New = New
data Edit = Edit Draft [ExtractorDraft]
data ExtractorView = ExtractorView ExtractorDraft
data FilenameTemplateResponse = FilenameTemplateResponse Text
data NextTemplateResponse = NextTemplateResponse Text
data ContentTemplateResponse = ContentTemplateResponse Text

instance ToMarkup Index where
    toMarkup _ = index

instance ToMarkup New where
    toMarkup _ = new

instance ToMarkup Edit where
    toMarkup (Edit d es) = draftView d es

instance ToMarkup ExtractorView where
    toMarkup (ExtractorView e) = extractorView e

instance ToMarkup FilenameTemplateResponse where
    toMarkup (FilenameTemplateResponse m) = toMarkup m

instance ToMarkup NextTemplateResponse where
    toMarkup (NextTemplateResponse m) = toMarkup m

instance ToMarkup ContentTemplateResponse where
    toMarkup (ContentTemplateResponse m) = toMarkup m

type API = Get '[HTML] Index
        :<|> "new" :> Get '[HTML] New
        :<|> "edit" :> Capture "id" DraftId :> Get '[HTML] Edit
        :<|> "draft" :>
               ( Capture "id" DraftId :>
                   ( "filename" :> ReqBody '[JSON] NewFilenameTemplateRequest :> Put '[HTML] FilenameTemplateResponse
                :<|> "next" :> ReqBody '[JSON] NewNextTemplateRequest :> Put '[HTML] NextTemplateResponse
                :<|> "content" :> ReqBody '[JSON] NewContentTemplateRequest :> Put '[HTML] ContentTemplateResponse
                )
            :<|> "extractor" :>
                   ( Capture "id" ExtractorDraftId :> ReqBody '[JSON] EditExtractorRequest :> Put '[HTML] ExtractorView
                :<|> Capture "id" ExtractorDraftId :> Delete '[HTML] NoContent
                :<|> "new" :> Capture "draft_id" DraftId :> Get '[HTML] ExtractorView
                )
            )
        :<|> "static" :> Raw


server :: Connection -> Server API
server conn = do
         return Index
    :<|> return New
    :<|> (editController conn)
    :<|> draftController
    :<|> serveDirectoryWebApp "static/src"
    where draftController =
                 templateController
            :<|> extractorController
          extractorController =
                 (editExtractor conn)
            :<|> (deleteExtractor conn)
            :<|> (newExtractor conn)
          templateController draftId =
                 (editFilename conn draftId)
            :<|> (editNext conn draftId)
            :<|> (editContent conn draftId)


editController :: Connection -> DraftId -> Handler Edit
editController conn draftId = do
    draft <- liftIO $ runSelectOne conn $ getDraftForId draftId
    case draft of
        Nothing -> throwError err404 { errBody = "Could not find draft with such id" }
        Just d -> do
            extractors <- liftIO $ runSelectList conn $ getExtractorsForDraft d
            extractorsWithSelectors <- forM extractors $ liftIO . updateSelectorCache conn -- Will update cache for all extractors on page reload
                                                                                           -- Possibly add an invalidation flag to filter updates
                                                                                           -- On the other hand, there could be only so many selectors on 1 page
                                                                                           -- And considering that this application is supposed to be self-hosted
                                                                                           -- This problem is not that serious
            return $ Edit d extractorsWithSelectors


updateSelectorCache :: Connection -> ExtractorDraft -> IO ExtractorDraft
updateSelectorCache conn extractor = do
    selectors <- runSelectList conn $ getSelectors extractor
    selectorClasses <- forM selectors $ runSelectList conn . getSelectorClasses
    let selector = buildSelector selectors selectorClasses
    let newEx = extractor { _extractorDraftSelectorCache = selector }
    runAndReturn (runUpdate conn . saveExtractorDraft) newEx

editExtractor :: Connection -> ExtractorDraftId -> EditExtractorRequest -> Handler ExtractorView
editExtractor conn exId (EditExtractorRequest newName newType) = do
    extractor <- liftIO $ runSelectOne conn $ getExtractorDraft exId
    ExtractorView <$> case extractor of
        Nothing -> throwError err404 { errBody = "Could not find extractor with such id" }
        Just ex -> do
            let newEx = ex { _extractorDraftName = newName, _extractorDraftType = stringMaybe newType }
            runAndReturn (liftIO . runUpdate conn . saveExtractorDraft) newEx

deleteExtractor :: Connection -> ExtractorDraftId -> Handler NoContent
deleteExtractor conn exId = do
    extractorExists <- liftIO $ runCheckExistance conn $ extractorDraftExists exId
    case extractorExists of
        False -> throwError err404 { errBody = "Could not find extractor with such id" }
        True -> liftIO $ runDelete conn $ deleteExtractorDraft exId
    return NoContent

newExtractor :: Connection -> DraftId -> Handler ExtractorView
newExtractor conn draftId = do
    draft <- liftIO $ runSelectOne conn $ getDraftForId draftId
    ExtractorView <$> case draft of
        Nothing -> throwError err404 { errBody = "Could not find draft with such id" }
        Just d -> liftIO $ runCreateOne conn $ createExtractorDraft d

editFilename :: Connection -> DraftId -> NewFilenameTemplateRequest -> Handler FilenameTemplateResponse
editFilename conn draftId (NewFilenameTemplateRequest template) = do
    _ <- updateDraft conn draftId (\d -> d { _draftTemplateFilename = template })
    -- Run template
    return $ FilenameTemplateResponse "Saved"

editNext :: Connection -> DraftId -> NewNextTemplateRequest -> Handler NextTemplateResponse
editNext conn draftId (NewNextTemplateRequest template) = do
    _ <- updateDraft conn draftId (\d -> d { _draftTemplateNext = template })
    -- Run template
    return $ NextTemplateResponse "Saved"

editContent :: Connection -> DraftId -> NewContentTemplateRequest -> Handler ContentTemplateResponse
editContent conn draftId (NewContentTemplateRequest template) = do
    _ <- updateDraft conn draftId (\d -> d { _draftTemplateContent = template })
    -- Run template
    return $ ContentTemplateResponse "Saved"

updateDraft :: Connection -> DraftId -> (Draft -> Draft) -> Handler Draft
updateDraft conn draftId update = do
    draft <- liftIO $ runSelectOne conn $ getDraftForId draftId
    case draft of
        Nothing -> throwError err404 { errBody = "Could not find draft with such id" }
        Just d -> runAndReturn (liftIO . runUpdate conn . saveDraft) $ update d

apiProxy :: Proxy API
apiProxy = Proxy

app :: Connection -> Application
app = serve apiProxy . server
