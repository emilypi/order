-- |
-- Module       : Data.Order.Partial
-- Copyright    : (c) 2020-2021 Emily Pillmore, Davean Scies
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Davean Scies <davean@xkcd.com>,
--                Siddharth Bhat <siddu.druid@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'PartialOrd', the class
-- of types that form a partial order.
--
module Data.Order.Partial
( PartialOrd
) where


import Data.Order.Pre
import Data.Void
import Data.Set
import Data.IntSet

-- | Adds a law to a preorder where we must have antisymmetry.
-- Law: forall a b. (a <= b) => !(b <= a)
-- @topos: if we are willing to have an `Eq` instance, then we can say:
--    (a <= b /\ b <= a) => (a = b)
class PreOrd a => PartialOrd a where

instance PartialOrd Void
instance PartialOrd ()
instance PartialOrd Bool
instance Ord a => PartialOrd (Set a)
instance PartialOrd IntSet
instance PartialOrd a => PartialOrd (Maybe a)

instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b)
instance (PartialOrd a, PartialOrd b, PartialOrd c) => PartialOrd (a, b, c)
instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d) => PartialOrd (a, b, c, d)
instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e) => PartialOrd (a, b, c, d, e)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f) => PartialOrd (a, b, c, d, e, f)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g) =>
         PartialOrd (a, b, c, d, e, f, g)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h) =>
         PartialOrd (a, b, c, d, e, f, g, h)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i) =>
         PartialOrd (a, b, c, d, e, f, g, h, i)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j)
instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j, PartialOrd k) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j, k)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j, PartialOrd k, PartialOrd l) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j, k, l)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j, PartialOrd k, PartialOrd l, PartialOrd m) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j, PartialOrd k, PartialOrd l, PartialOrd m, PartialOrd n) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j, k, l, m, n)


instance (PartialOrd a, PartialOrd b, PartialOrd c, PartialOrd d, PartialOrd e, PartialOrd f, PartialOrd g, PartialOrd h, PartialOrd i, PartialOrd j, PartialOrd k, PartialOrd l, PartialOrd m, PartialOrd n, PartialOrd o) =>
         PartialOrd (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
