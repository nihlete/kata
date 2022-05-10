module Lib () where

import Data.Foldable ()
import Data.Semigroup ( Endo(Endo, appEndo))

newtype MyMin a = MyMin {getMin :: Maybe a}

instance Ord a => Semigroup (MyMin a) where
  (MyMin Nothing) <> (MyMin Nothing) = MyMin Nothing
  (MyMin a) <> (MyMin Nothing) = MyMin a
  (MyMin Nothing) <> (MyMin b) = MyMin b
  (MyMin (Just a)) <> (MyMin (Just b)) = if a < b then MyMin (Just a) else MyMin (Just b)

instance Ord a => Monoid (MyMin a) where
  mempty = MyMin Nothing

myToList :: Foldable t => t a -> [a]
myToList = foldMap (: [])

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMin . foldMap (MyMin . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f x0 xs = appEndo (foldMap (Endo . f) xs) x0