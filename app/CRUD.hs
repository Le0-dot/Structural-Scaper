module CRUD where

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Data.Text
import Data.Int
import Models



createRecipe :: (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) => Text -> Text -> Int32 -> SqlInsert be RecipeT
createRecipe name host delayMs =
    insert (_scraperRecipe scraperDb) $
    insertExpressions [ Recipe default_ (val_ name) (val_ host) (val_ delayMs) (val_ False)]

createRecipeDraft :: (BeamSqlBackend be, BeamSqlBackendCanSerialize be Text) => Text -> Text -> Int32 -> SqlInsert be RecipeT
createRecipeDraft name host delayMs =
    insert (_scraperRecipe scraperDb) $
    insertExpressions [ Recipe default_ (val_ name) (val_ host) (val_ delayMs) (val_ True)]

createExtractor :: (BeamSqlBackend be, BeamSqlBackendCanSerialize be String, BeamSqlBackendCanSerialize be Text) => Text -> Text -> ExtractorType -> RecipeId -> SqlInsert be ExtractorT
createExtractor name selector extractorType recipe =
    insert (_scraperExtractor scraperDb) $
    insertExpressions [ Extractor default_ (val_ name) (val_ selector) (val_ extractorType) (val_ recipe) ]


getRecipes :: HasQBuilder be => SqlSelect be Recipe
getRecipes =
        select $ filter_ (not_ . _recipeIsDraft) $
        all_ (_scraperRecipe scraperDb)


runCreate :: Connection -> SqlInsert Sqlite a -> IO ()
runCreate conn = runBeamSqliteDebug putStrLn conn . runInsert

runSelectList :: FromBackendRow Sqlite a => Connection -> SqlSelect Sqlite a -> IO [a]
runSelectList conn = runBeamSqliteDebug putStrLn conn . runSelectReturningList
