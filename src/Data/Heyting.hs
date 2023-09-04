{-# language Safe #-}
-- |
-- Module       : Data.Heyting
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Heyting' algebras and their
-- associated combinators.
--
module Data.Heyting
( -- * Heyting algebras
  Heyting(..)
, (→)
, not
, (¬)
, const
) where

import Prelude ()

import Data.Lattice
import Data.Semilattice

-- | A @Heyting@ algebra is a 'BoundedLattice' such that for every
-- pair of elements in the algebra, there exists a unique greatest element
-- @x@ such that @a ∧ x <= b@ holds. We call @x@ the /relative pseudo-complement/
-- of @a@ with respect to @b@, and it is denoted @a '→' b@.
--
-- Generally, we call @('→')@ /implication/, and say that a @Heyting@
-- algebra is a 'BoundedLattice' for which implication holds.
--
-- Implication is the weakest proposition for which /modus ponens/
-- is valid as an inference rule.
--
-- Laws:
--
-- [Relative pseudo-complement] @a ∧ x <= b@
--
class BoundedLattice a => Heyting a where
  implies :: a -> a -> a
  {-# minimal implies #-}


-- | A unicode alias for 'implies'
--
(→) :: Heyting a => a -> a -> a
(→) = implies
infixr 0 →

-- | Negation in a 'Heyting' algebra is an implication @a '→' '⊥'@.
--
not :: Heyting a => a -> a
not a = implies a bottom

-- | A unicode alias for 'not'.
(¬) :: Heyting a => a -> a
(¬) = not

-- | The constant morphism from any element to the top element.
--
const :: Heyting a => a -> a
const _ = top
