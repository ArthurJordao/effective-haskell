{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}

module Typeclasses
  ( MaybeWrapper,
    MaybeWrapperNullable,
  )
where

import Prelude hiding (null)

class (Eq a) => Nullable a where
  isNull :: a -> Bool
  isNull a = a == null
  null :: a

instance (Eq a) => Nullable (Maybe a) where
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a, b) where
  null = (null, null)

instance (Eq a) => Nullable [a] where
  null = []

newtype MaybeWrapper a
  = MaybeWrapper (Maybe a)
  deriving (Nullable) via (Maybe a)
  deriving (Eq)

newtype MaybeWrapperNullable a
  = MaybeWrapperNullable (Maybe a)
  deriving (Eq)

instance (Nullable a) => Nullable (MaybeWrapperNullable a) where
  isNull (MaybeWrapperNullable Nothing) = True
  isNull (MaybeWrapperNullable (Just x)) = isNull x
  null = MaybeWrapperNullable (Just null)

newtype MyNewBrandNewestMaybe a = MyNewBrandNewestMaybe (Maybe a)
  deriving (Nullable) via (MaybeWrapperNullable a)
  deriving (Eq)
