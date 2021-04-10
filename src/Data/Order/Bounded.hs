{-# LANGUAGE DefaultSignatures #-}
-- |
-- Module       : Data.Order.Bounded
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Infimum', the class of
-- greatest lower bounds, and 'Superemum', the class of least
-- upper bounds.
--
module Data.Order.Bounded where

import Data.Void
import Data.Function (fix)
import Data.Proxy
import Data.Set
import Data.IntSet

-- | Infimum, also known as a "greatest lower bound", represents
-- the largest element of an ordered set that is also smaller than
-- all other inhabitants.
--
class Infimum a where
  inf :: a
  default inf :: Bounded a => a
  inf = minBound

instance Infimum ()
instance Infimum Bool
instance Infimum a => Infimum (Proxy a)
instance Infimum Void where
  inf = fix absurd
instance Ord a => Infimum (Set a) where
  inf = mempty
instance Infimum IntSet where
  inf = mempty

instance Infimum (Maybe a) where
  inf = Nothing

-- | Supremum, also known as a "least upper bound", represents
-- the smallest element of an ordered set that is also larger than
-- all other inhabitants.
--
class Supremum a where
  sup :: a
  default sup :: Bounded a => a
  sup = maxBound


instance Supremum ()
instance Supremum a => Supremum (Proxy a)
instance Supremum Bool
instance Supremum Void where
  sup = fix absurd
instance (Supremum a, Supremum b) => Supremum (a,b) where
  sup = (sup, sup)
instance Supremum a => Supremum (Maybe a) where
  sup = Just sup
