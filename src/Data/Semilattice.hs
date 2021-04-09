module Data.Semilattice where

import Data.Order.Bounded
import Data.Order.Partial
import Data.Void

-- @topos: "join" clashes with Control.Monad.join but importing as Lattice we get Lattice.join which I like anyway.
-- I think with the aliases this situation is "fine".
-- harmless' choice of the unicode symbols for this have some appeal to avoid the clash but OTOH I don't type
-- unicode without impedence.

-- | A join-semilattice. An associative idempotent commutative operator called `join` with
-- The `<=` operator from the partial order witnesses that join is
-- non-decreasing:
--
-- Associative:
--   forall a b c . a `join` (b `join` c) = (a `join` b) `join` c
-- Idempotent:
--   forall a. a `join` a = a
-- Commutative:
--   forall a b. a `join` b = b `join` a
-- Join is non-decreasing:
-- forall a b. a <= a `join` b
class PartialOrd a => Join a where
  join :: a -> a -> a

-- | A meet-semilattice. An associative idempotent commutative operator called `meet` with
-- The `<=` operator from the partial order
-- witnesses that the meet is non-inceasing.
-- Associative:
--   forall a b c . a `meet` (b `meet` c) = (a `meet` b) `meet` c
-- Idempotent:
--   forall a. a `meet` a = a
-- Commutative:
--   forall a b. a `meet` b = b `meet` a
-- Meet is non-increasing:
-- forall a b. a >= a `meet` b
class PartialOrd a => Meet a where
  meet :: a -> a -> a

-- @topos: I based these fixities off the boolean operations they relate to for bools in the Haskell report, thoughts?
infixr 2 \/
(\/), (∨) :: Join a => a -> a -> a
(\/) = join
(∨) = join

infixr 3 /\
(/\),(∧) :: Meet a => a -> a -> a
(/\) = meet
(∧) = meet

class (Infimum a, Join a) => BoundedJoin a where

bottom :: BoundedJoin a => a
bottom = inf

class (Supremum a, Meet a) => BoundedMeet a where

top :: BoundedMeet a => a
top = sup

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
