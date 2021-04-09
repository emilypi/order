{-# language Safe #-}
-- |
-- Module       : Data.Lattice
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Lattice's and their
-- bounded variants, along with associated combinators.
--
module Data.Lattice
( -- * Lattices
  Lattice(..)
, BoundedLattice(..)
) where


import Data.Semilattice


-- | A lattice is a 'Poset' that admits all finite 'meet's and 'join's.
-- It can also be defined algebraically as having two binary operations,
-- ∧ and ∨, which form a meet and join-semilattice respectively, along
-- with their absorption laws:
--
-- [Absorption 1] @a '∨' (a '∧' b) = a@
-- [Absorption 2] @a '∧' (a '∨' b) = a@
--
class (Join a, Meet a) => Lattice a where


-- | A bounded lattice is a 'Lattice' that admits a top and bottom element
-- such that the 'join' and 'meet' operations form 'BoundedJoin' and
-- 'BoundedMeet' semilattices respectively.
--
class (BoundedJoin a, BoundedMeet a) => BoundedLattice a where
