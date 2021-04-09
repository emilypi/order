{-# language Safe #-}
-- |
-- Module       : Data.Semilattice
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Join' and 'Meet' semilattices,
-- along with their bounded variants, and associated combinators.
--
module Data.Semilattice
( -- * Join semilattices
  Join(..)
, (\/)
, (∨)
  -- ** Bounded join semilattices
, BoundedJoin(..)
, bottom
, (⊥)
  -- * Meet semilattices
, Meet(..)
, (/\)
, (∧)
  -- ** Bounded meet semilattices
, BoundedMeet(..)
, top
, (⊤)
) where

import Data.Order.Bounded
import Data.Order.Partial
import Data.Void

-- -------------------------------------------------------------------- --
-- Join Semilattices

-- | A join-semilattice is a 'Poset' with an associative, idempotent, and
-- commutative binary operation called @join@.
--
-- Laws:
--
-- [Associative] @a '∨' (b '∨' c) = (a '∨' b) '∨' c@
-- [Idempotent] @a '∨' a = a@
-- [Commutative] @a '∨' b = b '∨' a@
-- [Non-decreasing] @a <= a '∨' b@
--
-- Alternatively, one can view a join semilattice as a commutative
-- 'Band' with respect to its 'join' operation.
--
class PartialOrd a => Join a where
  -- | The join operation of a join-semilattice.
  --
  join :: a -> a -> a

-- | A infix alias for 'join'
--
(\/) :: Join a => a -> a -> a
(\/) = join
infixr 6 \/

-- | An infix unicode alias for 'join'
--
(∨) :: Join a => a -> a -> a
(∨) = join
infixr 6 ∨

-- | A bounded join-semilattice is a join-semilattice that is bounded,
-- meaning that it admits a greatest lower bound (also known as a
-- bottom element, infimum), which is a unit for the 'join' operation.
--
-- Laws:
--
-- [Two-sided unital element] @a '∨' '⊥' = '⊥' '∨' a = a@
--
class (Infimum a, Join a) => BoundedJoin a where

-- | An alias for the bottom element of a 'BoundedJoin' semilattice.
--
bottom :: BoundedJoin a => a
bottom = inf

-- | A unicode alias for the bottom element of a
-- 'BoundedJoin' semilattice.
--
(⊥) :: BoundedJoin a => a
(⊥) = bottom

-- -------------------------------------------------------------------- --
-- Meet Semilattices

-- | A meet-semilattice is a 'Poset' with an associative, idempotent, and
-- commutative operator called @meet@.
--
-- Laws:
--
-- [Associative] @a '∧' (b '∧' c) = (a '∧' b) '∧' c@
-- [Idempotent] @a '∧' a = a@
-- [Commutative] @a '∧' b = b '∧' a@
-- [Non-increasing] @a '∧' b '<=' a@
--
-- Alternatively, one can view a meet semilattice as a commutative 'Band' with
-- respect to its 'meet' operation.
--
class PartialOrd a => Meet a where
  -- | The meet operation of a meet-semilattice.
  --
  meet :: a -> a -> a

-- | A infix alias for 'meet'
--
(/\)  :: Meet a => a -> a -> a
(/\) = meet
infixr 7 /\

-- | A unicode infix alias for 'meet'
--
(∧) :: Meet a => a -> a -> a
(∧) = meet
infixr 7 ∧

-- | A bounded meet-semilattice is a meet-semilattice that is bounded,
-- meaning that it admits a least upper bound (also known as a
-- top element, supremum), which is a unit for the 'meet' operation.
--
-- Laws:
--
-- [Two-sided unital element] @a '∧' '⊤' = '⊤' '∧' a = a@
--
class (Supremum a, Meet a) => BoundedMeet a where

-- | An alias for the top element of a 'BoundedMeet' semilattice.
--
top :: BoundedMeet a => a
top = sup

-- | A unicode alias for the top element of a 'BoundedMeet'
-- semilattice.
--
(⊤) :: BoundedMeet a => a
(⊤) = top

instance Join Void where
  join = const

instance Meet Void where
  meet = const

instance Join () where
  join _ _ = ()

instance Meet () where
  meet _ _ = ()

instance BoundedJoin ()
instance BoundedMeet ()

instance Join Bool where
  join = (||)

instance Meet Bool where
  meet = (&&)
