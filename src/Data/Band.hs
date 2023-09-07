-- |
-- Module       : Data.Band
-- Copyright    : (c) 2020-2023 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Band', the class
-- of idempotent semigroups.
--
module Data.Band
( Band(..)
) where


import Data.Semigroup

-- | @Band@s are type of idempotent semigroups.
--
class Semigroup a => Band a

instance Band All
instance Band Any
instance Band ()
instance Band a => Band (Dual a)
instance (Band a, Band b) => Band (a, b)
instance (Band a, Band b, Band c) => Band (a, b, c)
instance (Band a, Band b, Band c, Band d) => Band (a, b, c, d)
instance (Band a, Band b, Band c, Band d, Band e) => Band (a, b, c, d, e)
