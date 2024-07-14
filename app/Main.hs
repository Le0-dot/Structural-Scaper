{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Network.Wai.Handler.Warp
import CRUD
import API

main :: IO ()
main = testDB >> testServer


testDB :: IO ()
testDB = do
    conn <- open "testDB.db"
    d <- runCreateOne conn $ createDraft "testDraftRecipe" "asdf.asdf" 5000 "asfd" "zxcv" "asdf"
    ed <- runCreateOne conn $ createExtractorDraft "" (Nothing) d
    s <- runCreateOne conn $ createSelector "div" True (Just "asdf") True ed
    runCreate conn $ createSelectorClass "foo" True s

    putStrLn $ show ed

    close conn

testServer :: IO ()
testServer = do
    putStrLn "Server strated"
    withConnection "testDB.db" $
        run 8000 . app
