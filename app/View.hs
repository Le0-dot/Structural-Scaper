{-# LANGUAGE OverloadedStrings #-}

module View where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx


index :: Html
index = docTypeHtml $ do
    H.head $ do
        H.title "Structural Scraper"
    H.body $ do
        H.span "WellCum"


scriptText :: Html
scriptText = "function processInput(event) { event.target[0].value = encodeURIComponent(event.target[0].value) }"

new :: Html
new = docTypeHtml $ do
    script scriptText ! type_ "text/javascript"
    H.head $ do
        H.title "Structural Scraper - new"
    body $ do
        H.form ! onsubmit "processInput" ! action "/init" ! A.id "form" $ do
            H.label "URL: " ! for "url"
            input ! type_ "url" ! A.id "url" ! name "url" ! required "required"
            H.label "Delay: " ! for "delay"
            select ! name "delay" ! A.id "delay" $ do
                option "None" ! value "0"
                option "1s" ! value "1000"
                option "3s" ! value "3000"
                option "5s" ! value "5000"
            br
            input ! type_ "submit" ! value "Open"
