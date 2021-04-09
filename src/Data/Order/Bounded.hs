{-# LANGUAGE DefaultSignatures #-}
module Data.Order.Bounded where

class Infimum a where
  inf :: a
  default inf :: Bounded a => a
  inf = minBound

class Supremum a where
  sup :: a
  default sup :: Bounded a => a
  sup = maxBound

instance Infimum ()
instance Supremum ()

instance Infimum Bool
instance Supremum Bool

