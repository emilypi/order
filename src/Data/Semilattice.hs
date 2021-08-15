{-# language Trustworthy #-}
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
, Joins(..)
, (\/)
, (∨)
  -- ** Bounded join semilattices
, BoundedJoin(..)
, bottom
, (⊥)
  -- * Meet semilattices
, Meet(..)
, Meets(..)
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
import Data.Set
import qualified Data.Set as Set
import Data.IntSet
import Control.Applicative
import qualified Data.IntSet as IntSet

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
  {-# minimal join #-}

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


instance Join Void where
  join = const

instance Join () where
  join _ _ = ()

instance Join Bool where
  join = (||)

instance (Join a, Join b) => Join (a,b) where
  join (a,b) (c,d) = (join a c, join b d)

instance Ord a => Join (Set a) where
  join = Set.union

instance Join IntSet where
  join = IntSet.union

instance Join a => Join (Maybe a) where
  join = liftA2 join

-- | A bounded join-semilattice is a join-semilattice that is bounded,
-- meaning that it admits a greatest lower bound (also known as a
-- bottom element, infimum), which is a unit for the 'join' operation.
--
-- Laws:
--
-- [Two-sided unital element] @a '∨' '⊥' = '⊥' '∨' a = a@
--
class (Infimum a, Join a) => BoundedJoin a where

instance BoundedJoin ()
instance BoundedJoin Void
instance Ord a => BoundedJoin (Set a)
instance BoundedJoin IntSet
instance Join a => BoundedJoin (Maybe a)

-- | An alias for the bottom element of a 'BoundedJoin' semilattice.
--
bottom :: BoundedJoin a => a
bottom = inf

-- | A unicode alias for the bottom element of a
-- 'BoundedJoin' semilattice.
--
(⊥) :: BoundedJoin a => a
(⊥) = bottom

-- | Newtype wrapper yielding the underlying 'Semigroup'
-- and 'Monoid' instances for instances of 'Join'.
--
newtype Joins a = Joins { unJoin :: a }

instance Join a => Semigroup (Joins a) where
  Joins a <> Joins b = Joins (a \/ b)

instance BoundedJoin a => Monoid (Joins a) where
  mempty = Joins bottom

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
  {-# minimal meet #-}

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

instance Meet Void where
  meet = const

instance Meet () where
  meet _ _ = ()

instance Meet Bool where
  meet = (&&)

instance (Meet a, Meet b) => Meet (a,b) where
  meet (a,b) (c,d) = (meet a c, meet b d)

instance Ord a => Meet (Set a) where
  meet = Set.intersection

instance Meet IntSet where
  meet = IntSet.intersection

instance Meet a => Meet (Maybe a) where
  meet = liftA2 meet

-- | A bounded meet-semilattice is a meet-semilattice that is bounded,
-- meaning that it admits a least upper bound (also known as a
-- top element, supremum), which is a unit for the 'meet' operation.
--
-- Laws:
--
-- [Two-sided unital element] @a '∧' '⊤' = '⊤' '∧' a = a@
--
class (Supremum a, Meet a) => BoundedMeet a where


instance BoundedMeet ()
instance BoundedMeet Bool
instance (BoundedMeet a, BoundedMeet b) => BoundedMeet (a,b)
instance BoundedMeet a => BoundedMeet (Maybe a)


-- | An alias for the top element of a 'BoundedMeet' semilattice.
--
top :: BoundedMeet a => a
top = sup

-- | A unicode alias for the top element of a 'BoundedMeet'
-- semilattice.
--
(⊤) :: BoundedMeet a => a
(⊤) = top

-- | Newtype wrapper yielding the underlying 'Semigroup'
-- and 'Monoid' instances for instances of 'Meet'.
--
newtype Meets a = Meets { unMeet :: a }

instance Meet a => Semigroup (Meets a) where
  Meets a <> Meets b = Meets (a /\ b)

instance BoundedMeet a => Monoid (Meets a) where
  mempty = Meets top
