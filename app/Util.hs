{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Maybe (fromMaybe)
import Data.Composition ((.:.))
import Control.Conditional (guard)
import Control.Arrow ((&&&))
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

maybeEquals :: Eq a => a -> Maybe a -> Bool
maybeEquals left right = fromMaybe False $ (left ==) <$> right

toMaybe :: Bool -> a -> Maybe a
toMaybe b a = guard b *> pure a

toMaybeList :: (a -> Bool) -> (a -> b) -> [a] -> [Maybe b]
toMaybeList f g = map $ uncurry toMaybe . (f &&& g)

foldZipWith :: (Monoid m) => (a -> b -> m) -> [a] -> [b] -> m
foldZipWith = foldl1 (<>) .:. zipWith


newtype StringMaybe a = StringMaybe { stringMaybe :: Maybe a }
    deriving (Eq, Read, Show, Functor, Applicative, Monad, Semigroup, Monoid, Generic)

instance ToJSON a => ToJSON (StringMaybe a) where
    toJSON = maybe "" toJSON . stringMaybe

instance FromJSON a => FromJSON (StringMaybe a) where
    parseJSON "" = pure $ StringMaybe Nothing
    parseJSON x = StringMaybe <$> parseJSON x
