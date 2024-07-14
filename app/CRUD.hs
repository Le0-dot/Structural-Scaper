{-# LANGUAGE MonoLocalBinds #-}

module CRUD where

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Data.Maybe
import Data.Text (Text)
import Data.Int
import Models


createRecipe ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> Int32 -> Text -> Text -> Text -> SqlInsert be RecipeT
createRecipe name host delayMs filename content next =
    insert (_scraperRecipe scraperDb) $
    insertExpressions [ Recipe default_ (val_ name) (val_ host) (val_ delayMs) (val_ filename) (val_ content) (val_ next) ]

createExtractor ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be String, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> ExtractorType -> Recipe -> SqlInsert be ExtractorT
createExtractor name selector extractorType recipe =
    insert (_scraperExtractor scraperDb) $
    insertExpressions [ Extractor default_ (val_ name) (val_ selector) (val_ extractorType) (val_ $ pk recipe) ]

createDraft ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> Int32 -> Text -> Text -> Text -> SqlInsert be DraftT
createDraft name host delayMs filename content next =
    insert (_scraperDraft scraperDb) $
    insertExpressions [ Draft default_ (val_ name) (val_ host) (val_ delayMs) (val_ filename) (val_ content) (val_ next) ]

createExtractorDraft ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be (Maybe ExtractorType), BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe Text)) =>
    Text -> Maybe ExtractorType -> Draft -> SqlInsert be ExtractorDraftT
createExtractorDraft name extractorType draft =
    insert (_scraperExtractorDraft scraperDb) $
    insertExpressions [ ExtractorDraft default_ (val_ name) (val_ Nothing) (val_ extractorType) (val_ $ pk draft) ]

createSelector ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe Text)) =>
    Text -> Bool -> Maybe Text -> Bool -> ExtractorDraft -> SqlInsert be SelectorT
createSelector tag isTaken tagId idIsTaken extractorDraft =
    insert (_scraperSelector scraperDb) $
    insertExpressions [ Selector default_ (val_ tag) (val_ isTaken) (val_ tagId) (val_ $ isJust tagId && idIsTaken) (val_ $ pk extractorDraft) ]

createSelectorClass ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Bool -> Selector -> SqlInsert be SelectorClassT
createSelectorClass classname isTaken selector =
    insert (_scraperSelectorClass scraperDb) $
    insertExpressions [ SelectorClass default_ (val_ classname) (val_ isTaken) (val_ $ pk selector) ]


draftToRecipe ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Draft -> SqlInsert be RecipeT
draftToRecipe draft =
    createRecipe
        (_draftName draft)
        (_draftHost draft)
        (_draftDelayMs draft)
        (_draftTemplateFilename draft)
        (_draftTemplateContent draft)
        (_draftTemplateNext draft)



getRecipes :: HasQBuilder be => SqlSelect be Recipe
getRecipes = select $ all_ (_scraperRecipe scraperDb)

getDrafts :: HasQBuilder be => SqlSelect be Draft
getDrafts = select $ all_ (_scraperDraft scraperDb)

getDraftForId :: (HasQBuilder be, HasSqlEqualityCheck be Int32) => Int32 -> SqlSelect be Draft
getDraftForId draftId =
    select $ filter_ (\d -> _draftId d ==. val_ draftId) $
    all_ (_scraperDraft scraperDb)

getExtractorsForRecipe ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text) =>
    Recipe -> SqlSelect be Extractor
getExtractorsForRecipe recipe = select $ oneToMany_ (_scraperExtractor scraperDb) _extractorForRecipe (val_ recipe)

getExtractorsForDraft ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text) =>
    Draft -> SqlSelect be ExtractorDraft
getExtractorsForDraft draft = select $ oneToMany_ (_scraperExtractorDraft scraperDb) _extractorDraftForDraft (val_ draft)

getExtractorDraft ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32) =>
    Int32 -> SqlSelect be ExtractorDraft
getExtractorDraft exId = select
    $ filter_ (\ex -> _extractorDraftId ex ==. val_ exId)
    $ all_ (_scraperExtractorDraft scraperDb)



getSelectors ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe ExtractorType), BeamSqlBackendCanSerialize be (Maybe Text)) =>
    ExtractorDraft -> SqlSelect be Selector
getSelectors extractorDraft = select $ oneToMany_ (_scraperSelector scraperDb) _selectorForExtractorDraft (val_ extractorDraft)

getSelectorClasses ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe Text)) =>
    Selector -> SqlSelect be SelectorClass
getSelectorClasses selector = select $ oneToMany_ (_scraperSelectorClass scraperDb) _selectorClassForSelector (val_ selector)



saveExtractorDraft ::
    (HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be (Maybe Text), BeamSqlBackendCanSerialize be (Maybe ExtractorType), BeamSqlBackendCanSerialize be Text) =>
    ExtractorDraft -> SqlUpdate be ExtractorDraftT
saveExtractorDraft = save (_scraperExtractorDraft scraperDb)

updateExtractorDraft ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe ExtractorType)) =>
    Int32 -> Text -> Maybe ExtractorType -> SqlUpdate be ExtractorDraftT
updateExtractorDraft exId newName newType = update (_scraperExtractorDraft scraperDb)
    (\ex -> mconcat
            [ _extractorDraftName ex <-. val_ newName
            , _extractorDraftType ex <-. val_ newType
            ])
    (\ex -> _extractorDraftId ex ==. val_ exId)


runCreate :: Connection -> SqlInsert Sqlite a -> IO ()
runCreate conn = runBeamSqliteDebug putStrLn conn . runInsert

runCreateList :: (Beamable a, FromBackendRow Sqlite (a Identity)) => Connection -> SqlInsert Sqlite a -> IO [a Identity]
runCreateList conn = runBeamSqliteDebug putStrLn conn . runInsertReturningList

runCreateOne :: (Beamable a, FromBackendRow Sqlite (a Identity)) => Connection -> SqlInsert Sqlite a -> IO (a Identity)
runCreateOne conn q = do
    [r] <- runCreateList conn q
    return r

runSelectList :: FromBackendRow Sqlite a => Connection -> SqlSelect Sqlite a -> IO [a]
runSelectList conn = runBeamSqliteDebug putStrLn conn . runSelectReturningList

runSelectOne :: FromBackendRow Sqlite a => Connection -> SqlSelect Sqlite a -> IO (Maybe a)
runSelectOne conn = runBeamSqliteDebug putStrLn conn . runSelectReturningOne

runUpdate :: Connection -> SqlUpdate Sqlite a -> IO ()
runUpdate conn = runBeamSqliteDebug putStrLn conn . Database.Beam.runUpdate
