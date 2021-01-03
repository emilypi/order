module Data.Heyting where
import Data.Lattice

-- | A bounded lattice equipped with an implication operation that behaves
-- akin to implication in classical logic: 
--   `a => b = not a || b`.
-- However, since we are defining these over an arbitrary bounded lattice and not
-- a boolean algebra, we do not have access to  the `not` operation. Hence,
-- we axiomatize implication with the following laws:
--
-- 1. Identity function is top:
--    a `imply` a = top 
--
-- 2. (@topos: what's a good name for this? I think of it as function application)
--    a `meet` (a `imply` b) = a `meet` b 

-- 3. (@topos: what's a good name for this? I don't grok this condition)
--   b `meet` (a `imply` b) = b
--
-- 4. Distributivity of `=>` over `meet`: 
--   a `imply` (b `meet` c) = (a `imply` b) `meet` (a `imply` c) 
class BoundedLattice a => Heytig a where
  imply :: a -> a -> a
