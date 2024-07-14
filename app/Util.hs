{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Maybe (fromMaybe)
import Control.Conditional (guard)
import Control.Arrow ((&&&))
import Data.Aeson (FromJSON(..), ToJSON(..))
import GHC.Generics (Generic)

maybeEquals :: Eq a => a -> Maybe a -> Bool
maybeEquals left right = fromMaybe False $ (left ==) <$> right

toMaybe :: Bool -> a -> Maybe a
toMaybe b a = guard b *> pure a

toMaybePred :: (a -> Bool) -> a -> Maybe a
toMaybePred p = uncurry toMaybe . (p &&& id)

toMaybeList :: (a -> Bool) -> (a -> b) -> [a] -> [Maybe b]
toMaybeList f g = map $ uncurry toMaybe . (f &&& g)

runAndReturn :: Monad m => (a -> m ()) -> a -> m a
runAndReturn a v = a v >> return v


newtype StringMaybe a = StringMaybe { stringMaybe :: Maybe a }
    deriving (Eq, Read, Show, Functor, Applicative, Monad, Semigroup, Monoid, Generic)

instance ToJSON a => ToJSON (StringMaybe a) where
    toJSON = maybe "" toJSON . stringMaybe

instance FromJSON a => FromJSON (StringMaybe a) where
    parseJSON "" = pure $ StringMaybe Nothing
    parseJSON x = StringMaybe <$> parseJSON x
