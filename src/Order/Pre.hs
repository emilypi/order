module Order.Pre where

-- | A preorder is an ordering that is reflexive and transitive, but not
-- necessarily antisymmetric.
-- reflexivity:
--   forall a. a <= a
-- transitivity: 
--   forall a b c. (a <= b && b <= c) => (a <= c)
class PreOrd a where
  leq ::  a -> a -> Bool
