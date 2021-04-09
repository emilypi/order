module Data.Semilattice where

import Data.Order.Bounded
import Data.Order.Partial
import Data.Void

-- | A join-semilattice. An associative idempotent commutative operator called `join` with
-- The `<=` operator from the partial order witnesses that join is
-- non-decreasing:
--
-- [Associative] @∀ a b c. a ∨ (b ∨ c) = (a ∨ b) ∨ c@
-- [Idempotent] @∀ a. a ∨ a = a@
-- [Commutative] @∀ a b. a ∨ b = b ∨ a@
-- [Non-decreasing] @{Poset p} => ∀ a b. a <= a ∨ b@
--
-- Alternatively, one can view a join semilattice as a commutative 'Band' with
-- respect to its 'join' operation.
--
class PartialOrd a => Join a where
  join :: a -> a -> a

-- | A meet-semilattice. An associative idempotent commutative operator called `meet` with
-- The `>=` operator from the partial order witnesses that meet is
-- non-increasing:
--
-- [Associative] @∀ a b c. a ∧ (b ∧ c) = (a ∧ b) ∧ c@
-- [Idempotent] @∀ a. a ∧ a = a@
-- [Commutative] @∀ a b. a ∧ b = b ∧ a@
-- [Non-increasing] @{Poset p} => ∀ a b. a >= a ∧ b@
--
-- Alternatively, one can view a meet semilattice as a commutative 'Band' with
-- respect to its 'meet' operation.
--
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
