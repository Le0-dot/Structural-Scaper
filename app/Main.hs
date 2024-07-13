{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple
import Network.Wai.Handler.Warp
import Models
import CRUD
import API

main :: IO ()
main = testDB >> testServer


testDB :: IO ()
testDB = do
    conn <- open "testDB.db"
    r <- runCreateOne conn $ createRecipe "testRecipe" "example.com" 3000 "{{ filename }}" "{{ text }}" "{{ url }}"
    putStrLn $ show r
    runCreate conn $ createExtractor "testExtractor" "div#id.asdf" TextType r
    d <- runCreateOne conn $ createDraft "testDraftRecipe" "asdf.asdf" 5000 "asfd" "zxcv" "asdf"
    ed <- runCreateOne conn $ createExtractorDraft "testDraftRecipe" Nothing d
    s <- runCreateOne conn $ createSelector "div" False (Just "asdf") True ed
    ss <- runCreateOne conn $ createSelectorClass "foo" True s

    putStrLn $ show $ buildTagSelector s [ss]
    close conn

testServer :: IO ()
testServer = do
    putStrLn "Server strated"
    withConnection "testDB.db" $
        run 8080 . app
