module Data.Lattice where
import Data.Semilattice

-- A join and a meet semilattice which interact correctly
-- forall a b. a `join` (a `meet` b) = `a`
-- forall a b. a `meet` (a `join` b) = a
class (JoinSemilattice a, MeetSemilattice a) => Lattice a where


-- | bot is an identity for `join`, and `top` is an identity for `meet`.
class Lattice a => BoundedLattice a where
  bot :: a 
  top :: a
