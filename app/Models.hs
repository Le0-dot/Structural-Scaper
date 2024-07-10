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
import Data.Text (Text)
import Data.Int


data RecipeT f = Recipe
        { _recipeId      :: C f Int32
        , _recipeName    :: C f Text
        , _recipeHost    :: C f Text
        , _recipeDelayMs :: C f Int32
        , _recipeIsDraft :: C f Bool
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
                deriving (Show, Read, Eq, Ord, Enum)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be ExtractorType where
  sqlValueSyntax = autoSqlValueSyntax


data TemplateT f = Template
        { _templateId        :: C f Int32
        , _templateFilename  :: C f Text
        , _templateContent   :: C f Text
        , _templateNext      :: C f Text
        , _templateForRecipe :: PrimaryKey RecipeT f
        } deriving (Generic, Beamable)

type Template = TemplateT Identity
deriving instance Show Template

instance Table TemplateT where
    data PrimaryKey TemplateT f = TemplateId (C f Int32)
                deriving (Generic, Beamable)
    primaryKey = TemplateId . _templateId

type TemplateId = PrimaryKey TemplateT Identity
deriving instance Show TemplateId


data ExtractorDraftT f = ExtractorDraft
        { _extractorDraftId        :: C f Int32
        , _extractorDraftName      :: C f (Maybe Text)
        , _extractorDraftType      :: C f (Maybe ExtractorType)
        , _extractorDraftForRecipe :: PrimaryKey RecipeT f
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
        , _scraperTemplate       :: f (TableEntity TemplateT)
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
                , _scraperTemplate =
                    modifyTableFields
                        tableModification { _templateForRecipe = RecipeId "recipe" }
                , _scraperExtractorDraft =
                    modifyTableFields
                        tableModification
                            { _extractorDraftName = "name"
                            , _extractorDraftType = "type"
                            , _extractorDraftForRecipe = RecipeId "recipe"
                            }
                , _scraperSelector =
                    modifyTableFields
                        tableModification { _selectorForExtractorDraft = ExtractorDraftId "extractor_draft" }
                , _scraperSelectorClass =
                    modifyTableFields
                        tableModification
                            { _selectorClassValue = "value"
                            , _selectorClassIsTaken = "is_taken"
                            , _selectorClassForSelector = SelectorId "selector"
                            }
                }
