module Order.Partial where
import Order.Pre

-- | Adds a law to a preorder where we must have antisymmetry.
-- Law: forall a b. (a <= b) => !(b <= a)
-- @topos: if we are willing to have an `Eq` instance, then we can say:
--    (a <= b /\ b <= a) => (a = b)
class PreOrd a => PartialOrd a where
