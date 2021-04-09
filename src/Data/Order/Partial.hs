module Data.Order.Partial where

import Data.Order.Pre
import Data.Void

-- | Adds a law to a preorder where we must have antisymmetry.
-- Law: forall a b. (a <= b) => !(b <= a)
-- @topos: if we are willing to have an `Eq` instance, then we can say:
--    (a <= b /\ b <= a) => (a = b)
class PreOrd a => PartialOrd a where

instance PartialOrd Void
instance PartialOrd ()
instance PartialOrd Bool