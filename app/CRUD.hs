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
    Text -> Text -> Int32 -> SqlInsert be RecipeT
createRecipe name host delayMs =
    insert (_scraperRecipe scraperDb) $
    insertExpressions [ Recipe default_ (val_ name) (val_ host) (val_ delayMs) (val_ False)]

createRecipeDraft ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> Int32 -> SqlInsert be RecipeT
createRecipeDraft name host delayMs =
    insert (_scraperRecipe scraperDb) $
    insertExpressions [ Recipe default_ (val_ name) (val_ host) (val_ delayMs) (val_ True)]

createExtractor ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be String, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> ExtractorType -> Recipe -> SqlInsert be ExtractorT
createExtractor name selector extractorType recipe =
    insert (_scraperExtractor scraperDb) $
    insertExpressions [ Extractor default_ (val_ name) (val_ selector) (val_ extractorType) (val_ $ pk recipe) ]

createTemplate ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Text -> Text -> Recipe -> SqlInsert be TemplateT
createTemplate filename content next recipe =
    insert (_scraperTemplate scraperDb) $
    insertExpressions [ Template default_ (val_ filename) (val_ content) (val_ next) (val_ $ pk recipe) ]

createExtractorDraft ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be (Maybe ExtractorType), BeamSqlBackendCanSerialize be Text) =>
    Text -> Maybe ExtractorType -> Recipe -> SqlInsert be ExtractorDraftT
createExtractorDraft name extractorType recipe =
    insert (_scraperExtractorDraft scraperDb) $
    insertExpressions [ ExtractorDraft default_ (val_ name) (val_ extractorType) (val_ $ pk recipe) ]

createSelector ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe Text)) =>
    Text -> Maybe Text -> Bool -> ExtractorDraft -> SqlInsert be SelectorT
createSelector tag tagId isTaken extractorDraft =
    insert (_scraperSelector scraperDb) $
    insertExpressions [ Selector default_ (val_ tag) (val_ tagId) (val_ $ isJust tagId && isTaken) (val_ $ pk extractorDraft) ]

createSelectorClass ::
    (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) =>
    Text -> Bool -> Selector -> SqlInsert be SelectorClassT
createSelectorClass classname isTaken selector =
    insert (_scraperSelectorClass scraperDb) $
    insertExpressions [ SelectorClass default_ (val_ classname) (val_ isTaken) (val_ $ pk selector) ]


getRecipes :: HasQBuilder be => SqlSelect be Recipe
getRecipes =
    select $ filter_ (not_ . _recipeIsDraft) $
    all_ (_scraperRecipe scraperDb)

getRecipeDrafts :: HasQBuilder be => SqlSelect be Recipe
getRecipeDrafts =
    select $ filter_ _recipeIsDraft $
    all_ (_scraperRecipe scraperDb)

getDataForRecipe ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text) =>
    Recipe -> SqlSelect be (Extractor, Template)
getDataForRecipe recipe = select $ do
    extractor <- oneToOne_ (_scraperExtractor scraperDb) _extractorForRecipe (val_ recipe)
    template <- oneToOne_ (_scraperTemplate scraperDb) _templateForRecipe (val_ recipe)
    return (extractor, template)

getDataForRecipeDraft ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text) =>
    Recipe -> SqlSelect be (ExtractorDraft, Template)
getDataForRecipeDraft recipe = select $ do
    extractor <- oneToOne_ (_scraperExtractorDraft scraperDb) _extractorDraftForRecipe (val_ recipe)
    template <- oneToOne_ (_scraperTemplate scraperDb) _templateForRecipe (val_ recipe)
    return (extractor, template)

getSelectors ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe ExtractorType)) =>
    ExtractorDraft -> SqlSelect be Selector
getSelectors extractorDraft = select $ oneToMany_ (_scraperSelector scraperDb) _selectorForExtractorDraft (val_ extractorDraft)

getSelectorClasses ::
    (HasQBuilder be, HasSqlEqualityCheck be Int32, BeamSqlBackendCanSerialize be Text, BeamSqlBackendCanSerialize be (Maybe Text)) =>
    Selector -> SqlSelect be SelectorClass
getSelectorClasses selector = select $ oneToMany_ (_scraperSelectorClass scraperDb) _selectorClassForSelector (val_ selector)


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
