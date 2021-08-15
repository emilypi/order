{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module       : Data.Band.Rectangular
-- Copyright    : (c) 2011 Edward Kmett
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for rectangular 'Band's, the class idempotent semigroups which are nowhere
-- commutative.
--
module Data.Band.Rectangular
( Band(..)
) where


import Data.Data
import Data.Band

import GHC.Generics

-- | a rectangular band is a nowhere commutative semigroup.
-- That is to say, if ab = ba then a = b. From this it follows
-- classically that aa = a and that such a band is isomorphic
-- to the following structure
--
data Rect i j = Rect i j
  deriving
    ( Eq, Ord, Show, Read
    , Generic, Generic1
    , Data, Typeable
    )

instance Semigroup (Rect i j) where
  Rect i _ <> Rect _ j = Rect i j

instance Band (Rect i j)
