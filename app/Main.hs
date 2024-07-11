{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Models
import CRUD

main :: IO ()
main = testDB


testDB :: IO ()
testDB = do
    conn <- open "testDB.db"
    newRecipes <- runCreateList conn $ createRecipe "testRecipe" "example.com" 3000
    putStrLn $ show newRecipes
    f:_ <- runSelectList conn getRecipes
    runCreate conn $ createExtractor "testExtractor" "div#id.asdf" TextType f
    runCreate conn $ createTemplate "{{ filename }}" "{{ text }}" "{{ url }}" f
    ed <- runCreateOne conn $ createExtractorDraft "testDraftRecipe" Nothing f
    s <- runCreateOne conn $ createSelector "div" (Just "asdf") True ed
    runCreate conn $ createSelectorClass "foo" False s

    (Just (e, t)) <- runSelectOne conn $ getDataForRecipe f

    putStrLn $ show e
    putStrLn $ show t

    return ()
