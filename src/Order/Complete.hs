{-# LANGUAGE TypeFamilies #-}
module Order.Complete where
import Order.Partial

-- | Davey and Priestly, 2nd edition, Definition 7.7: Directed set:
-- let S be a non-empty subset of an ordered set P.
-- Then ∀ x, y ∈ S, ∃z ∈ S, z ∈ x ≤ z ∧ y ≤ z

-- | Davey and Priestly, 2nd edition, Definition 8.1: a CPO is an ordered set with a bottom, 
-- where each directed set has a supremum.
class PartialOrd a => CompletePartialOrd a where
   -- | conceivably, some types may have better encodings of directed subsets
   -- than others..?
   type Directed a -- associated type of directed subsets?
   bot :: a
   sup :: Directed a -> a

-- @topos: I recall that it's enough to ask for all /chains/ to possess a sup,
-- and not directed sets, and everything of value works out in this case.
-- Davey and Priestly, 2nd edition, section 8.10 says: > It turns out that an
-- ordered set is a CPO provided that each chain has a least upper bound in P.
-- (Note that the join of the empty chain guarantess the existence of _|_) We
-- omit the proof of this highly nontrivial result, which we record below as a
-- t-heorem. Exercise 8.9 seeks a proof in the countable case; the general case
-- requires the machinery of ordinals.
--
-- @topos: What's a useful encoding of directed sets in the finite/computable
-- case for us?

