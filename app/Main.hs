{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite
import Models

main :: IO ()
main = testDB


testDB :: IO ()
testDB = do
    conn <- open "testDB.db"
    runBeamSqliteDebug putStrLn conn $ runInsert $
        insert (_scraperRecipes scraperDb) $
        insertExpressions [ Recipe default_ (val_ "testRecipe") (val_ "example.com") (val_ 3000) ]
    return ()
