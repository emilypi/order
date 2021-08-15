{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language Trustworthy #-}
-- |
-- Module       : Data.Ordered.Directed
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions the newtype associated with
-- the union (joins) of upwards directed sets and the intersection
-- (meets) of downward directed sets.
--
module Data.Order.Directed
( -- * The lattice of directed sets
  Directed(..)
  -- ** Aliases
, Upwards
, Downwards
) where


import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Semilattice
import Data.Order.Pre
import Data.Order.Partial
import Data.Order.Bounded
import Data.Lattice

-- | Wrapper for the lattice of upwards and downwards directed sets.
--
newtype Directed = Directed { getDirected :: Ordering }
  deriving
    ( Eq, Show, Read
    , Bounded, Enum
    , Data, Typeable
    , Generic
    )

instance PreOrd Directed where
  leq (Directed a) (Directed b) = a `leq` b

instance PartialOrd Directed

instance Join Directed where
  join (Directed LT) a = a
  join a (Directed LT) = a
  join (Directed EQ) a = a
  join a (Directed EQ) = a
  join a _ = a

instance Infimum Directed where
  inf = Directed LT

instance BoundedJoin Directed

instance Meet Directed where
  meet (Directed GT) a = a
  meet a (Directed GT) = a
  meet (Directed EQ) a = a
  meet a (Directed EQ) = a
  meet a _ = a

instance Supremum Directed where
  sup = Directed GT

instance BoundedMeet Directed

instance Lattice Directed

instance BoundedLattice Directed

-- | The semilattice of upwards directed sets, closed under joins (union)
--
type Upwards = Joins Directed

-- | The semilattice of downwards directed sets closed under meets
-- (intersection)
--
type Downwards = Meets Directed
