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


data ScraperDB f = ScraperDB
        { _scraperRecipes    :: f (TableEntity RecipeT)
        , _scraperExtractors :: f (TableEntity ExtractorT)
        , _scraperTemplates  :: f (TableEntity TemplateT)
        } deriving (Generic, Database be)

scraperDb :: DatabaseSettings be ScraperDB
scraperDb = defaultDbSettings `withDbModification`
            dbModification
                { _scraperRecipes =
                    setEntityName "recipe" <>
                    modifyTableFields
                        tableModification
                            { _recipeId      = "id"
                            , _recipeName    = "name"
                            , _recipeHost    = "host"
                            , _recipeDelayMs = "delay_ms"
                            }
                , _scraperExtractors =
                    setEntityName "extractor" <>
                    modifyTableFields
                        tableModification
                            { _extractorId        = "id"
                            , _extractorName      = "name"
                            , _extractorSelector  = "selector"
                            , _extractorType      = "type"
                            , _extractorForRecipe = RecipeId "recipe"
                            }
                , _scraperTemplates =
                    setEntityName "tempalte" <>
                    modifyTableFields
                        tableModification
                            { _templateId        = "id"
                            , _templateFilename  = "filename"
                            , _templateContent   = "content"
                            , _templateNext      = "next"
                            , _templateForRecipe = RecipeId "recipe"
                            }
                }
