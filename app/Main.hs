{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite
import Models
import CRUD

main :: IO ()
main = testDB


testDB :: IO ()
testDB = do
    conn <- open "testDB.db"
    runCreate conn $ createRecipe "testRecipe" "example.com" 3000
    recipes <- runSelectList conn getRecipes
    putStrLn $ show recipes
    runCreate conn $ createExtractor "testExtractor" "div#id.asdf" TextType $ RecipeId 1
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperExtractor scraperDb) $
        insertExpressions [ Extractor default_ (val_ "testExtractor") (val_ "div#id") (val_ TextType) (RecipeId 1) ]
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperTemplate scraperDb) $
        insertExpressions [ Template default_ (val_ "{{ file }}") (val_ "{{ text }}") (val_ "{{ url }}") (RecipeId 1) ]
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperExtractorDraft scraperDb) $
        insertExpressions [ ExtractorDraft default_ (val_ $ Just "testDraftRecipe") (val_ Nothing) (RecipeId 1) ]
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperSelector scraperDb) $
        insertExpressions [ Selector default_ (val_ "div") (val_ $ Just "asdf") (val_ True) (ExtractorDraftId 1) ]
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperSelectorClass scraperDb) $
        insertExpressions [ SelectorClass default_ (val_ "foo") (val_ False) (SelectorId 1) ]
    return ()
