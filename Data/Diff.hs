{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Data.Diff where

import Import

import Text.Blaze
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (map)

import Data.Foldable

import qualified Data.Map.Lazy as LM

data Diff a = OneThing a | TwoThings a a


presentDiff :: (Eq a, IsString m, Monoid m) => (a -> m) -> (m -> m) -> (m -> m) -> (Diff a -> m)
presentDiff presentAtom _ _ (OneThing u) = presentAtom u
presentDiff presentAtom presentOld presentNew (TwoThings old new)
  | old == new = presentAtom new
  | otherwise = presentOld (presentAtom old) <> presentNew (presentAtom new)


instance (Eq a, Show a) => Show (Diff a) where
  show d = presentDiff show
                       (\x -> "[-" ++ x ++ "-]")
                       (\x -> "{+" ++ x ++ "+}")
                       d

instance (Eq a, ToMarkup a) => ToMarkup (Diff a) where
  toMarkup d = presentDiff toMarkup
                           (Text.Blaze.Html4.Strict.span ! (Text.Blaze.Html4.Strict.Attributes.style "color:red;text-decoration: line-through;"))
                           (Text.Blaze.Html4.Strict.span ! (Text.Blaze.Html4.Strict.Attributes.style "color:green;text-decoration: underline;"))
                           d

  -- toMarkup (OneThing u) = toMarkup u
  -- toMarkup (TwoThings old new) = ((Text.Blaze.Html4.Strict.span ! (Text.Blaze.Html4.Strict.Attributes.style "color:green;")) (toMarkup new)) <> " (" <> ((Text.Blaze.Html4.Strict.span ! (Text.Blaze.Html4.Strict.Attributes.style "color:red;")) (toMarkup old)) <> ")"


instance Functor Diff where
  fmap fun (OneThing u) = OneThing (fun u)
  fmap fun (TwoThings old new) = TwoThings (fun old) (fun new)

instance Foldable Diff where
  foldMap f (OneThing u) = f u
  foldMap f (TwoThings old new) = f old `mappend` f new

instance Traversable Diff where
  traverse f (OneThing u) = OneThing <$> f u
  traverse f (TwoThings old new) = TwoThings <$> f old <*> f new

current :: Diff a -> a
current (OneThing u) = u
current (TwoThings _ new) = new

class Diffable t where
  type DiffSettings t
  type DiffResult t
  diff :: DiffSettings t -> t -> t -> DiffResult t
  single :: t -> DiffResult t
  runDiff :: DiffSettings t -> Diff t -> DiffResult t
  runDiff _ (OneThing u) = single u
  runDiff s (TwoThings old new) = diff s old new

instance Diffable Int where
  type DiffSettings Int = ()
  type DiffResult Int = Diff Int
  single u = OneThing u
  diff _ old new
    | old == new = OneThing new
    | otherwise = TwoThings old new

instance Diffable Text where
  type DiffSettings Text = ()
  type DiffResult Text = Diff Text
  single u = OneThing u
  diff _ old new
    | old == new = OneThing new
    | otherwise = TwoThings old new

instance Diffable t => Diffable (Maybe t) where
  type DiffSettings (Maybe t) = (t, DiffSettings t)
  type DiffResult (Maybe t) = Maybe (DiffResult t)
  single Nothing = Nothing
  single (Just u) = Just $ single u
  diff (_, sub) (Just old) (Just new) = Just $ diff sub old new
  diff (defaultValue, sub) (Just old) Nothing = Just $ diff sub old defaultValue
  diff (defaultValue, sub) Nothing (Just new) = Just $ diff sub defaultValue new
  diff (_, _) Nothing Nothing = Nothing

instance (Eq v) => Diffable ([v]) where
  type DiffSettings ([v]) = ()
  type DiffResult ([v]) = [(v, Diff Bool)]
  single t = map (\e -> (e, OneThing True)) t
  diff () old new = [(oe, TwoThings True False) | oe <- old, not (oe `Import.elem` new) ]
                   ++ map (\ne -> (ne, if ne `Import.elem` old then OneThing True else TwoThings False True)) new

instance (Eq k, Ord k, Diffable v) => Diffable (LM.Map k v) where
  type DiffSettings (LM.Map k v) = (v, DiffSettings v)
  type DiffResult (LM.Map k v) = LM.Map k (DiffResult v)
  single m = LM.map single m
  diff (defaultValue, sub) old new = LM.mergeWithKey (\_ a b -> Just $ diff sub a b)
                                                     (LM.map (\x -> diff sub x defaultValue))
                                                     (LM.map (\x -> diff sub defaultValue x))
                                                     old
                                                     new
