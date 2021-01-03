module Data.Semilattice where
import Order.Partial

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
class PartialOrd a => JoinSemilattice a where
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
class PartialOrd a => MeetSemilattice a where
  meet :: a -> a -> a
