-- |
-- Module       : Data.Band
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
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

-- | @Band@s are type of idempotent semigroups.
--
class Semigroup a => Band a
