{-# LANGUAGE DefaultSignatures #-}
module Data.Order.Pre where

import Data.Void

-- | A preorder is an ordering that is reflexive and transitive, but not
-- necessarily antisymmetric.
-- reflexivity:
--   forall a. a <= a
-- transitivity: 
--   forall a b c. (a <= b && b <= c) => (a <= c)
class PreOrd a where
  leq ::  a -> a -> Bool
  default leq :: Ord a => a -> a -> Bool
  leq = (<=)

instance PreOrd Void

instance PreOrd () where
  leq _ _ = True

instance PreOrd Bool where
  