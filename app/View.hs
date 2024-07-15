{-# LANGUAGE OverloadedStrings #-}

module View where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Htmx
import Data.Text (Text)
import Models
import Control.Conditional ((?<>))
import qualified Data.Text as T
import Util
import Data.Maybe


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

extractorTypeSelected :: ExtractorType -> ExtractorDraft -> Attribute
extractorTypeSelected extractorType extractor = (maybeEquals extractorType $ _extractorDraftType extractor) ?<> (selected "selected")

extractorView :: ExtractorDraft -> Html
extractorView extractor = do
    H.div ! A.id thisId ! class_ "flex-col" $ do
        H.div ! class_ "flex" $
            inputElement "name" (_extractorDraftName extractor)
                ! A.id inputId
                ! class_ "grow"
                ! hxPut handleUrl
                ! hxTarget ("div#" <> thisId)
                ! hxSwap "outerHTML"
                ! hxInclude ("select#" <> selectId)
                ! onkeypress "return event.charCode != 32" -- Does not allow empty space
        H.div ! class_ "flex, flex-row" $ do
            H.span "Selector: " ! class_ "flex-none"
            H.span (toHtml $ fromMaybe "None" $ _extractorDraftSelectorCache extractor) ! class_ "grow"
            select
                ! A.id selectId
                ! name "value"
                ! hxExt "json-enc"
                ! hxPut handleUrl
                ! hxTarget ("div#" <> thisId)
                ! hxSwap "outerHTML"
                ! hxInclude ("input#" <> inputId)
                $ do
                option "None" ! value "" ! class_ "hidden"
                option "href" ! extractorTypeSelected Href extractor -- TODO: Check if tag is <a>
                option "text" ! extractorTypeSelected TextType extractor
                option "innerHTML" ! extractorTypeSelected InnerHTML extractor
                option "outerHTML" ! extractorTypeSelected OuterHTML extractor
        button "Delete"
            ! hxDelete handleUrl
            ! hxTarget ("div#" <> thisId)
            ! hxSwap "delete"
            ! hxConfirm "Are you sure? This action is not reversible."
    where exId = toValue $ _extractorDraftId extractor
          handleUrl = "/draft/extractor/" <> exId
          thisId = "extractor-" <> exId
          inputId = "name-" <> exId
          selectId = "value-" <> exId

draftView :: Draft -> [ExtractorDraft] -> Html
draftView draft extractors = docTypeHtml $ do
    H.head $ do
        linkTailwind
        linkHtmx
        H.title $ toHtml $ "Structural Scraper - edit " <> _draftName draft
    H.body ! class_ "flex flex-col h-full" $ do
        H.div ! class_ "flex grow" $ do
            H.div ! class_ "basis-1/2" $ do
                h2 "Extractors" ! class_ "text-center"
                H.div ! A.id "extractors" $ foldMap extractorView extractors
                H.div ! class_ "flex justify-center" $ do
                    button "Add extractor"
                        ! hxGet ("/draft/extractor/new/" <> dId)
                        ! hxTarget "div#extractors"
                        ! hxSwap "beforeend"
            H.div ! class_ "basis-1/2 flex flex-col" $ do
                h2 "Template" ! class_ "text-center"
                H.div ! class_ "flex" $ do
                    inputElement "filename" (_draftTemplateFilename draft)
                        ! class_ "basis-1/2"
                        ! hxPut ("/draft/" <> dId <> "/filename")
                        ! hxTarget "next"
                    H.div "" ! class_ "basis-1/2"
                H.div ! class_ "flex" $ do
                    inputElement "next" (_draftTemplateNext draft)
                        ! class_ "basis-1/2"
                        ! hxPut ("/draft/" <> dId <> "/next")
                        ! hxTarget "next"
                    H.div "" ! class_ "basis-1/2"
                H.div ! class_ "flex" $ do
                    textarea (toMarkup $ _draftTemplateContent draft)
                        ! class_ "resize-none basis-1/2"
                        ! A.id "content"
                        ! name "content"
                        ! placeholder "Content..."
                        ! hxExt "json-enc"
                        ! hxPut ("/draft/" <> dId <> "/content")
                        ! hxTrigger "input changed delay:1s"
                        ! hxTarget "next"
                    H.div "" ! class_ "basis-1/2"
        H.div ! class_ "flex justify-center" $ button "Save"
        H.div ! class_ "flex justify-center" $ H.span "" ! A.id "save-message"
    where dId = toValue $ _draftId draft


inputElement :: Text -> Text -> Html
inputElement elName elValue = input
    ! name (toValue elName)
    ! value (toValue elValue)
    ! placeholder (toValue $ T.toTitle elName <> "...")
    ! hxExt "json-enc"
    ! hxTrigger "input changed delay:500ms"
