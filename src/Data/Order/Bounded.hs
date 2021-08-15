{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
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

import           Control.Applicative
import           Control.Monad.ST

import qualified Data.Functor.Contravariant as Functor
import qualified Data.Functor.Identity as Functor
import           Data.Function (fix)
import           Data.Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Ord (Down(..))
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import           Data.Void
import           Data.Word

import           GHC.Generics

import           Numeric.Natural

#if (MIN_VERSION_base(4,15,0))
import GHC.Event
import GHC.Tuple
#endif


#include "HsBaseConfig.h"

-- | Infimum, also known as a "greatest lower bound", represents
-- the largest element of an ordered set that is also smaller than
-- all other inhabitants.
--
-- The default instance is selected to coencode with the Monoid
-- so that our BoundedJoin semilattices also coencide by default.
class Infimum a where
  inf :: a
  default inf :: Monoid a => a
  inf = mempty

-- | Supremum, also known as a "least upper bound", represents
-- the smallest element of an ordered set that is also larger than
-- all other inhabitants.
--
class Supremum a where
  sup :: a
  default sup :: Bounded a => a
  sup = maxBound

instance Infimum Ordering where
  inf = minBound
instance Supremum Ordering where
  sup = maxBound

instance Infimum Any
instance Supremum Any

instance Infimum All
instance Supremum All

#if (MIN_VERSION_base(4,15,0))
instance Infimum Lifetime
instance Supremum Lifetime where
  sup = MultiShot

instance Infimum Event

instance Infimum a => Infimum (Solo a) where
  inf = Solo inf
instance Supremum a => Supremum (Solo a) where
  sup = Solo sup
#endif

instance Infimum [a]

instance Infimum a => Infimum (IO a) where
  inf = pure inf
instance Supremum a => Supremum (IO a) where
  sup = pure sup

instance Infimum a => Infimum (ST s a) where
  inf = pure inf
instance Supremum a => Supremum (ST s a) where
  sup = pure sup

deriving newtype instance Infimum a => Infimum (Par1 a)
deriving newtype instance Supremum a => Supremum (Par1 a)

instance Supremum a => Infimum (Down a) where
  inf = Down sup
instance Infimum a => Supremum (Down a) where
  sup = Down inf

deriving newtype  instance Infimum a => Infimum (First a)

deriving newtype  instance Supremum a => Supremum (Last a)

deriving newtype instance Infimum a => Infimum (Functor.Identity a)
deriving newtype instance Supremum a => Supremum (Functor.Identity a)

deriving newtype instance Infimum a => Infimum (Max a)
deriving newtype instance Supremum a => Supremum (Max a)

deriving newtype instance Infimum a => Infimum (Min a)
deriving newtype instance Supremum a => Supremum (Min a)

instance Infimum (Functor.Equivalence a)
instance Infimum (Functor.Comparison a)
instance Infimum (Functor.Predicate a)

instance Infimum b => Infimum (a -> b) where
  inf = const inf
instance Supremum b => Supremum (a -> b) where
  sup = const sup

instance Infimum ()
instance Supremum ()

instance Infimum Bool where
  inf = minBound
instance Supremum Bool

instance Infimum a => Infimum (Proxy a)
instance Supremum a => Supremum (Proxy a)

instance Infimum Void where
  inf = fix absurd
instance Supremum Void where
  sup = fix absurd

deriving instance Infimum a => Infimum (Functor.Op a b)

deriving instance Infimum (f p) => Infimum (Rec1 f p)
deriving instance Supremum (f p) => Supremum (Rec1 f p)

instance Alternative f => Infimum (Monoid.Alt f a)
instance (Alternative f, Monoid a) => Infimum (Monoid.Ap f a)

deriving instance Infimum a => Infimum (Const a b)
deriving instance Supremum a => Supremum (Const a b)

deriving instance Infimum c => Infimum (K1 i c p)
deriving instance Supremum c => Supremum (K1 i c p)

instance (Infimum (f p), Infimum (g p)) => Infimum ((f :*: g) p) where
  inf = inf :*: inf
instance (Supremum (f p), Supremum (g p)) => Supremum ((f :*: g) p) where
  sup = sup :*: sup

deriving instance Infimum (f (g p)) => Infimum ((f :.: g) p)
deriving instance Supremum (f (g p)) => Supremum ((f :.: g) p)

deriving instance Infimum (f p) => Infimum (M1 i c f p)

instance (Infimum a, Infimum b) => Infimum (a, b) where
  inf = (inf, inf)
instance (Supremum a, Supremum b) => Supremum (a, b) where
  sup = (sup, sup)

instance (Infimum a, Infimum b, Infimum c) => Infimum (a, b, c) where
  inf = (inf, inf, inf)
instance (Supremum a, Supremum b, Supremum c) => Supremum (a,b, c) where
  sup = (sup, sup, sup)

instance (Infimum a, Infimum b, Infimum c, Infimum d) => Infimum (a, b, c, d) where
  inf = (inf, inf, inf, inf)
instance (Supremum a, Supremum b, Supremum c, Supremum d) => Supremum (a, b, c, d) where
  sup = (sup, sup, sup, sup)

instance (Infimum a, Infimum b, Infimum c, Infimum d, Infimum e) => Infimum (a, b, c, d, e) where
  inf = (inf, inf, inf, inf, inf)
instance (Supremum a, Supremum b, Supremum c, Supremum d, Supremum e) => Supremum (a, b, c, d, e) where
  sup = (sup, sup, sup, sup, sup)

instance Infimum (Maybe a) where
  inf = Nothing
instance Supremum a => Supremum (Maybe a) where
  sup = Just sup

instance Ord a => Infimum (Set.Set a)

instance Infimum IntSet.IntSet

instance Infimum (IntMap.IntMap v)

instance Ord k => Infimum (Map.Map k v)

instance Infimum Natural where
  inf = 0

instance Infimum Char where
  inf = minBound
instance Supremum Char where
  sup = maxBound

instance Infimum Int where
  inf = minBound
instance Supremum Int where
  sup = maxBound
instance Infimum Int8 where
  inf = minBound
instance Supremum Int8 where
  sup = maxBound
instance Infimum Int16 where
  inf = minBound
instance Supremum Int16 where
  sup = maxBound
instance Infimum Int32 where
  inf = minBound
instance Supremum Int32 where
  sup = maxBound
instance Infimum Int64 where
  inf = minBound
instance Supremum Int64 where
  sup = maxBound

instance Infimum Word where
  inf = minBound
instance Supremum Word where
  sup = maxBound
instance Infimum Word8 where
  inf = minBound
instance Supremum Word8 where
  sup = maxBound
instance Infimum Word16 where
  inf = minBound
instance Supremum Word16 where
  sup = maxBound
instance Infimum Word32 where
  inf = minBound
instance Supremum Word32 where
  sup = maxBound
instance Infimum Word64 where
  inf = minBound
instance Supremum Word64 where
  sup = maxBound
