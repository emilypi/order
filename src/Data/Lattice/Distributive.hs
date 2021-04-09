module Data.Lattice.Distributive where
import Data.Lattice

-- | a distributive lattice. `join` and `meet` distribute over each other:
--
-- Join distributes:
--   forall a b c. a `join` (b `meet` c) = (a `join` b) `meet` (a `join` c)
--
-- Meet distributes:
--   forall a b c. a `meet` (b `join` c) = (a `meet` b) `join` (a `meet` c)
class Lattice a => Distributive a where
