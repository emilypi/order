{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
, BoundedJoin
, bottom
, (⊥)
  -- * Meet semilattices
, Meet(..)
, Meets(..)
, (/\)
, (∧)
  -- ** Bounded meet semilattices
, BoundedMeet
, top
, (⊤)
) where

import           Control.Applicative
import           Control.Monad (liftM2)
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Bits
import           Data.Data
import           Data.Functor.Classes
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import           Data.Ix
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Monoid as Monoid
import           Data.Ord
import           Data.Order.Bounded
import           Data.Order.Pre
import           Data.Order.Partial
import           Data.Set
import           Data.Void
import           Foreign.Storable
import           GHC.Generics
import Data.IntSet

#if (MIN_VERSION_base(4,15,0))
import GHC.Event
import GHC.Tuple
#endif

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
class Join a where
  -- | The join operation of a join-semilattice.
  --
  join :: a -> a -> a
  default join :: Semigroup a => a -> a -> a
  join = (<>)
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

-- | Newtype wrapper yielding the underlying 'Semigroup'
-- and 'Monoid' instances for instances of 'Join'.
--
newtype Joins a
  = Joins { unJoin :: a }
  deriving
    ( Read, Show, Eq, Ord
    , Functor, Foldable, Traversable
    , Generic, Generic1, Typeable, Data
    )
  deriving anyclass (PreOrd, PartialOrd)
  deriving newtype
    ( Floating, Fractional, Num, Real, RealFloat, RealFrac
    , Ix, FiniteBits, Bits
    , Storable, Bounded, Enum
    )

instance Join a => Semigroup (Joins a) where
  Joins a <> Joins b = Joins (a \/ b)

instance BoundedJoin a => Monoid (Joins a) where
  mempty = Joins bottom

instance Applicative Joins where
  pure = Joins
  (Joins f) <*> (Joins v) = Joins (f v)

instance Monad Joins where
  Joins a >>= k = k a

instance MonadFix Joins where
  mfix f = Joins (fix (unJoin . f))

instance MonadZip Joins where
  mzipWith = liftM2

instance Eq1 Joins where
  liftEq eq (Joins x) (Joins y) = eq x y

instance Ord1 Joins where
  liftCompare comp (Joins x) (Joins y) = comp x y

instance Read1 Joins where
  liftReadsPrec rp _ = readsData $ readsUnaryWith rp "Joins" Joins

instance Show1 Joins where
  liftShowsPrec sp _ d (Joins x) = showsUnaryWith sp "Joins" d x

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
class Meet a where
  -- | The meet operation of a meet-semilattice.
  --
  meet :: a -> a -> a
  {-# minimal meet #-}

-- | A infix alias for 'meet'
--
(/\)  :: Meet a => a -> a -> a
(/\) = meet
infixr 7 /\\

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

-- | Newtype wrapper yielding the underlying 'Semigroup'
-- and 'Monoid' instances for instances of 'Meet'.
--
newtype Meets a
  = Meets { unMeet :: a }
  deriving
    ( Read, Show, Eq, Ord
    , Functor, Foldable, Traversable
    , Generic, Generic1, Typeable, Data
    )
  deriving anyclass (PreOrd, PartialOrd)
  deriving newtype
    ( Floating, Fractional, Num, Real, RealFloat, RealFrac
    , Ix, FiniteBits, Bits
    , Storable, Bounded, Enum
    )

instance Meet a => Semigroup (Meets a) where
  Meets a <> Meets b = Meets (a /\ b)

instance BoundedMeet a => Monoid (Meets a) where
  mempty = Meets top

instance Applicative Meets where
  pure = Meets
  (Meets f) <*> (Meets v) = Meets (f v)

instance Monad Meets where
  Meets a >>= k = k a

instance MonadFix Meets where
  mfix f = Meets (fix (unMeet . f))

instance MonadZip Meets where
  mzipWith = liftM2

instance Eq1 Meets where
  liftEq eq (Meets x) (Meets y) = eq x y

instance Ord1 Meets where
  liftCompare comp (Meets x) (Meets y) = comp x y

instance Read1 Meets where
  liftReadsPrec rp _ = readsData $ readsUnaryWith rp "Meet" Meets

instance Show1 Meets where
  liftShowsPrec sp _ d (Meets x) = showsUnaryWith sp "Meet" d x

instance Join Void where
  join = const

instance BoundedJoin Void

instance Meet Void where
  meet = const

instance BoundedMeet Void

instance Join () where
  join _ _ = ()

instance BoundedJoin ()

instance Meet () where
  meet _ _ = ()

instance BoundedMeet ()

instance Join Bool where
  join = (||)

instance BoundedJoin Bool

instance Meet Bool where
  meet = (&&)

instance BoundedMeet Bool

instance Join Ordering where
  join LT a = a
  join EQ LT = EQ
  join EQ a = a
  join GT _ = GT
instance BoundedJoin Ordering
instance Meet Ordering where
  meet GT a = a
  meet EQ GT = EQ
  meet EQ a = a
  meet LT _ = LT
instance BoundedMeet Ordering

deriving newtype instance Join Monoid.Any
instance Meet Monoid.Any where
  meet (Monoid.Any a) (Monoid.Any b) = Monoid.Any (a && b)
instance BoundedJoin Monoid.Any
instance BoundedMeet Monoid.Any

deriving newtype instance Join Monoid.All
instance Meet Monoid.All where
  meet (Monoid.All a) (Monoid.All b) = Monoid.All (a || b)
instance BoundedJoin Monoid.All
instance BoundedMeet Monoid.All

#if (MIN_VERSION_base(4,15,0))
instance Join Lifetime where
  join = (<>)
instance BoundedJoin Lifetime
instance Meet Lifetime where
  meet MultiShot MultiShot = MultiShot
  meet _ _ = OneShot
instance BoundedMeet Lifetime

instance Join Event where
  join = (<>)
instance BoundedJoin Event
#endif

deriving newtype instance Join a => Join (Par1 a)

instance (Join a, Infimum a) => BoundedJoin (Par1 a)

instance Join (NonEmpty a) where
  join = (<>)

instance Meet a => Join (Down a) where
  join (Down a) (Down b) = Down (meet a b)

instance (Meet a, Supremum a) => BoundedJoin (Down a)

instance Join a => Meet (Down a) where
  meet (Down a) (Down b) = Down (join a b)

instance (Join a, Infimum a) => BoundedMeet (Down a)

instance Ord a => Join (Set a) where
  join = Set.union

instance Ord a => BoundedJoin (Set a)

instance Ord a => Meet (Set a) where
  meet = Set.intersection

instance Join IntSet.IntSet where
  join = IntSet.union

instance BoundedJoin IntSet.IntSet

instance Meet IntSet.IntSet where
  meet = IntSet.intersection

instance Join v => Join (IntMap.IntMap v) where
  join = IntMap.unionWith join

instance Join v => BoundedJoin (IntMap.IntMap v)

instance Meet v => Meet (IntMap.IntMap v) where
  meet = IntMap.intersectionWith meet

instance (Ord k, Join v) => Join (Map.Map k v) where
  join = Map.unionWith join

instance (Ord k, Join v) => BoundedJoin (Map.Map k v)

instance (Ord k, Meet v) => Meet (Map.Map k v) where
  meet = Map.intersectionWith meet

instance Join a => Join (Maybe a) where
  join = liftA2 join

instance Join a => BoundedJoin (Maybe a)

instance Meet a => Meet (Maybe a) where
  meet = liftA2 meet

instance BoundedMeet a => BoundedMeet (Maybe a)

#if (MIN_VERSION_base(4,15,0))
instance (Join a) => Join (Solo a) where
  join (Solo a1) (Solo a2) = Solo (join a1 a2)

instance (Meet a) => Meet (Solo a) where
  meet (Solo a1) (Solo a2) = Solo (meet a1 a2)
#endif

instance (Join a, Join b) => Join (a,b) where
  join (a1,b1) (a2,b2) = (join a1 a2, join b1 b2)

instance (Meet a, Meet b) => Meet (a,b) where
  meet (a1,b1) (a2,b2) = (meet a1 a2, meet b1 b2)

instance (Join a, Join b, Join c) => Join (a,b,c) where
  join (a1,b1,c1) (a2,b2,c2) = (join a1 a2, join b1 b2, join c1 c2)

instance (Meet a, Meet b, Meet c) => Meet (a,b,c) where
  meet (a1,b1,c1) (a2,b2,c2) = (meet a1 a2, meet b1 b2, meet c1 c2)

instance (Join a, Join b, Join c, Join d) => Join (a,b,c,d) where
  join (a1,b1,c1,d1) (a2,b2,c2,d2) = (join a1 a2, join b1 b2, join c1 c2, join d1 d2)

instance (Meet a, Meet b, Meet c, Meet d) => Meet (a,b,c,d) where
  meet (a1,b1,c1,d1) (a2,b2,c2,d2) = (meet a1 a2, meet b1 b2, meet c1 c2, meet d1 d2)

instance (Join a, Join b, Join c, Join d, Join e) => Join (a,b,c,d,e) where
  join (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = (join a1 a2, join b1 b2, join c1 c2, join d1 d2, join e1 e2)

instance (Meet a, Meet b, Meet c, Meet d, Meet e) => Meet (a,b,c,d,e) where
  meet (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = (meet a1 a2, meet b1 b2, meet c1 c2, meet d1 d2, meet e1 e2)

instance (BoundedMeet a, BoundedMeet b) => BoundedMeet (a,b)
instance (BoundedMeet a, BoundedMeet b, BoundedMeet c) => BoundedMeet (a,b,c)
instance (BoundedMeet a, BoundedMeet b, BoundedMeet c, BoundedMeet d) => BoundedMeet (a,b,c,d)
instance (BoundedMeet a, BoundedMeet b, BoundedMeet c, BoundedMeet d, BoundedMeet e) => BoundedMeet (a,b,c,d,e)

instance (BoundedJoin a, BoundedJoin b) => BoundedJoin (a,b)
instance (BoundedJoin a, BoundedJoin b, BoundedJoin c) => BoundedJoin (a,b,c)
instance (BoundedJoin a, BoundedJoin b, BoundedJoin c, BoundedJoin d) => BoundedJoin (a,b,c,d)
instance (BoundedJoin a, BoundedJoin b, BoundedJoin c, BoundedJoin d, BoundedJoin e) => BoundedJoin (a,b,c,d,e)
