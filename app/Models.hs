{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad (guard)
import Data.Composition
import Data.Aeson (FromJSON)
import Util
import Data.Aeson.Types (FromJSON(..))


data RecipeT f = Recipe
        { _recipeId                :: C f Int32
        , _recipeName              :: C f Text
        , _recipeHost              :: C f Text
        , _recipeDelayMs           :: C f Int32
        , _recipeTemplateFilename  :: C f Text
        , _recipeTemplateContent   :: C f Text
        , _recipeTemplateNext      :: C f Text
        } deriving (Generic, Beamable)

type Recipe = RecipeT Identity
deriving instance Show Recipe

instance Table RecipeT where
    data PrimaryKey RecipeT f = RecipeId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = RecipeId . _recipeId

type RecipeId = PrimaryKey RecipeT Identity
deriving instance Show RecipeId


data ExtractorT f = Extractor
        { _extractorId        :: C f Int32
        , _extractorName      :: C f Text
        , _extractorSelector  :: C f Text
        , _extractorType      :: C f ExtractorType
        , _extractorForRecipe :: PrimaryKey RecipeT f
        } deriving (Generic, Beamable)

type Extractor = ExtractorT Identity
deriving instance Show Extractor

instance Table ExtractorT where
    data PrimaryKey ExtractorT f = ExtractorId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = ExtractorId . _extractorId

type ExtractorId = PrimaryKey ExtractorT Identity
deriving instance Show ExtractorId


data ExtractorType = Href | TextType | InnerHTML | OuterHTML
                deriving (Show, Read, Eq, Ord, Enum, Generic)


instance FromJSON ExtractorType where
    parseJSON "href" = pure Href
    parseJSON "text" = pure TextType
    parseJSON "innerHTML" = pure InnerHTML
    parseJSON "outerHTML" = pure OuterHTML
    parseJSON _ = fail "Not an extractor type"

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExtractorType where
    sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite ExtractorType where
    fromBackendRow = read . T.unpack <$> fromBackendRow



data DraftT f = Draft
        { _draftId                :: C f Int32
        , _draftName              :: C f Text
        , _draftHost              :: C f Text
        , _draftDelayMs           :: C f Int32
        , _draftTemplateFilename  :: C f Text
        , _draftTemplateContent   :: C f Text
        , _draftTemplateNext      :: C f Text
        } deriving (Generic, Beamable)

type Draft = DraftT Identity
deriving instance Show Draft

instance Table DraftT where
    data PrimaryKey DraftT f = DraftId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = DraftId . _draftId

type DraftId = PrimaryKey DraftT Identity
deriving instance Show DraftId


data ExtractorDraftT f = ExtractorDraft
        { _extractorDraftId            :: C f Int32
        , _extractorDraftName          :: C f Text
        , _extractorDraftSelectorCache :: C f (Maybe Text)
        , _extractorDraftType          :: C f (Maybe ExtractorType)
        , _extractorDraftForDraft      :: PrimaryKey DraftT f
        } deriving (Generic, Beamable)

type ExtractorDraft = ExtractorDraftT Identity
deriving instance Show ExtractorDraft

instance Table ExtractorDraftT where
    data PrimaryKey ExtractorDraftT f = ExtractorDraftId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = ExtractorDraftId . _extractorDraftId

type ExtractorDraftId = PrimaryKey ExtractorDraftT Identity
deriving instance Show ExtractorDraftId


data SelectorT f = Selector
        { _selectorId                :: C f Int32
        , _selectorTag               :: C f Text
        , _selectorTagIsTaken        :: C f Bool
        , _selectorTagId             :: C f (Maybe Text)
        , _selectorTagIdIsTaken      :: C f Bool
        , _selectorForExtractorDraft :: PrimaryKey ExtractorDraftT f
        } deriving (Generic, Beamable)

type Selector = SelectorT Identity
deriving instance Show Selector

instance Table SelectorT where
    data PrimaryKey SelectorT f = SelectorId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = SelectorId . _selectorId

type SelectorId = PrimaryKey SelectorT Identity
deriving instance Show SelectorId


data SelectorClassT f = SelectorClass
        { _selectorClassId          :: C f Int32
        , _selectorClassValue       :: C f Text
        , _selectorClassIsTaken     :: C f Bool
        , _selectorClassForSelector :: PrimaryKey SelectorT f
        } deriving (Generic, Beamable)

type SelectorClass = SelectorClassT Identity
deriving instance Show SelectorClass

instance Table SelectorClassT where
    data PrimaryKey SelectorClassT f = SelectorClassId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = SelectorClassId . _selectorClassId

type SelectorClassId = PrimaryKey SelectorClassT Identity
deriving instance Show SelectorClassId


data ScraperDB f = ScraperDB
        { _scraperRecipe         :: f (TableEntity RecipeT)
        , _scraperExtractor      :: f (TableEntity ExtractorT)
        , _scraperDraft          :: f (TableEntity DraftT)
        , _scraperExtractorDraft :: f (TableEntity ExtractorDraftT)
        , _scraperSelector       :: f (TableEntity SelectorT)
        , _scraperSelectorClass  :: f (TableEntity SelectorClassT)
        } deriving (Generic, Database be)

scraperDb :: DatabaseSettings be ScraperDB
scraperDb = defaultDbSettings `withDbModification`
            dbModification
                { _scraperExtractor =
                    modifyTableFields
                        tableModification { _extractorForRecipe = RecipeId "recipe" }
                , _scraperExtractorDraft =
                    modifyTableFields
                        tableModification
                            { _extractorDraftId = "id"
                            , _extractorDraftName = "name"
                            , _extractorDraftSelectorCache = "selector_cache"
                            , _extractorDraftType = "type"
                            , _extractorDraftForDraft = DraftId "draft"
                            }
                , _scraperSelector =
                    modifyTableFields
                        tableModification
                            { _selectorId = "id"
                            , _selectorForExtractorDraft = ExtractorDraftId "extractor_draft"
                            }
                , _scraperSelectorClass =
                    modifyTableFields
                        tableModification
                            { _selectorClassId = "id"
                            , _selectorClassValue = "value"
                            , _selectorClassIsTaken = "is_taken"
                            , _selectorClassForSelector = SelectorId "selector"
                            }
                }

buildTagSelector :: Selector -> [SelectorClass] -> Maybe Text
buildTagSelector selector selectorClasses =
    toMaybe (_selectorTagIsTaken selector) $ foldl1 (<>) [tag, formattedId, formattedClasses]
    where tag = _selectorTag selector
          tagId = guard (_selectorTagIdIsTaken selector) *> _selectorTagId selector
          classes = toMaybeList _selectorClassIsTaken _selectorClassValue selectorClasses
          formattedId = fromMaybe "" $ ("#" <>) <$> tagId
          formattedClasses = T.concat $ map ("." <>) $ catMaybes classes

buildSelector :: [Selector] -> [[SelectorClass]] -> Maybe Text
buildSelector = toMaybePred (not . T.null) . T.unwords . catMaybes .: zipWith buildTagSelector
