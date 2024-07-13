{-# LANGUAGE OverloadedStrings #-}

module View where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx
import Data.Text (Text)
import Models
import Control.Conditional (if', (?<>))
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Composition ((.:.))


linkTailwind :: Html
linkTailwind = link ! rel "stylesheet" ! href "/static/css/style.css"

linkHtmx :: Html
linkHtmx = script "" ! src "/static/js/htmx/htmx.min.js"
        <> script "" ! src "/static/js/htmx/json-enc.js"

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

maybeEquals :: Eq a => a -> Maybe a -> Bool
maybeEquals left right = fromMaybe False $ (left ==) <$> right

extractorTypeSelected :: ExtractorType -> ExtractorDraft -> Attribute
extractorTypeSelected extractorType extractor = (maybeEquals extractorType $ _extractorDraftType extractor) ?<> (selected "selected")

extractorView :: ExtractorDraft -> Text -> Html
extractorView extractor selector = docTypeHtml $ do
    H.div ! A.id "extractor" ! class_ "flex-col" $ do
        H.div ! class_ "flex" $
            inputElement "name" (_extractorDraftName extractor) ! class_ "grow"
        H.div ! class_ "flex, flex-row" $ do
            H.span "Selector: " ! class_ "flex-none"
            H.span (toHtml $ if' (T.null selector) "None" selector) ! class_ "grow"
            select ! name "value" $ do
                option "None" ! class_ "hidden"
                option "href" ! extractorTypeSelected Href extractor -- TODO: Check if tag is <a>
                option "text" ! extractorTypeSelected TextType extractor
                option "innerHTML" ! extractorTypeSelected InnerHTML extractor
                option "outerHTML" ! extractorTypeSelected OuterHTML extractor
        button "Delete"

foldZipWith :: (Monoid m) => (a -> b -> m) -> [a] -> [b] -> m
foldZipWith = foldl1 (<>) .:. zipWith

draftView :: Draft -> [ExtractorDraft] -> [Text] -> Html
draftView draft extractors selectors = docTypeHtml $ do
    H.head $ do
        linkTailwind
        linkHtmx
        H.title $ toHtml $ "Structural Scraper - edit " <> _draftName draft
    H.body ! class_ "flex flex-col h-full" $ do
        H.div ! class_ "flex grow" $ do
            H.div ! class_ "basis-1/2" $ do
                h2 "Extractors" ! class_ "text-center"
                H.div ! A.id "extractors" $ foldZipWith extractorView extractors selectors
                H.div ! class_ "flex justify-center" $ do
                    button "Add extractor"
            H.div ! class_ "basis-1/2 flex flex-col" $ do
                h2 "Template" ! class_ "text-center"
                H.div ! class_ "flex" $ do
                    inputElement "filename" (_draftTemplateFilename draft) ! class_ "grow"
                H.div ! class_ "flex" $ do
                    inputElement "next" (_draftTemplateNext draft) ! class_ "grow"
                H.div ! class_ "flex" $ do
                    textarea (toMarkup $ _draftTemplateContent draft)
                        ! class_ "resize-none grow"
                        ! A.id "content"
                        ! name "content"
                        ! placeholder "Content..."
        H.div ! class_ "flex justify-center" $ button "Save"
        H.div ! class_ "flex justify-center" $ H.span "" ! A.id "save-message"


inputElement :: Text -> Text -> Html
inputElement elName elValue = input
    ! A.id (toValue elName)
    ! name (toValue elName)
    ! value (toValue elValue)
    ! placeholder (toValue $ T.toTitle elName <> "...")
