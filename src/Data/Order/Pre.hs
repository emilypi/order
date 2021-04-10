{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Trustworthy #-}
module Data.Order.Pre
( -- * Preorders
  PreOrd(..)
, geq
, (>=)
, (≧)
, (<=)
, (≦)
  -- * Lifted classes
, PreOrd1(..)
) where

-- Some of these instances need to move to Poset because we have Eq reqirements
-- Maybe this means ParOrd1 is actually only Poset1?

import           Prelude hiding ((<=), (>=))

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception

import           Data.Array
import           Data.Char
import           Data.Fixed
import           Data.Foldable
import qualified Data.Functor.Compose  as Functor
import qualified Data.Functor.Const    as Functor
import qualified Data.Functor.Identity as Functor
import qualified Data.Functor.Product  as Functor
import qualified Data.Functor.Sum      as Functor
import           Data.Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Ord (Down(..))
import qualified Data.Ord as Ord
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup
import qualified Data.Set as Set
import           Data.Type.Coercion
import           Data.Type.Equality
import           Data.Unique
import           Data.Version
import           Data.Void
import           Data.Word

import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr

import           GHC.ByteOrder
import           GHC.Conc
import           GHC.Fingerprint.Type
import           GHC.Generics
import           GHC.TypeLits

import           Numeric.Natural

import           System.Exit
import           System.IO
import           System.Posix.Types

import           Type.Reflection

#if (MIN_VERSION_base(4,15,0))
import GHC.Event
import GHC.Tuple
#endif


#include "HsBaseConfig.h"


-- | A preordered set (aka a /proset/) is a set with an ordering
-- that is reflexive and transitive.
--
-- Laws:
--
-- [Reflexivity] @a '<=' a@
-- [Transitivity] @a '<=' b and b '<=' c implies a '<=' c
--
-- These conditions are necessary and sufficient to define a thin category
-- where reflexivity is the identity morphism, and between pairs
-- of elements, there is a single morphism.
--
-- Pre-orders differ from partial orders in that a pre-orders do not
-- necessarily have to be anti-symmetric: that is,
-- @a '<=' b and b '<=' a@ does not necessarily imply that @a '==' b@.
--
class PreOrd a where
  leq ::  a -> a -> Bool
  default leq :: Ord a => a -> a -> Bool
  leq = (Ord.<=)

-- | Alias for flipped 'leq'.
--
geq :: PreOrd a => a -> a -> Bool
geq = flip leq

-- | Infix operator alias for 'leq'.
--
(<=) :: PreOrd a => a -> a -> Bool
(<=) = leq
infix 4 <=

-- | Unicode infix alias for 'leq'.
--
(≦) :: PreOrd a => a -> a -> Bool
(≦) = leq
infix 4 ≦

-- | Infix operator alias for 'leq'.
--
(>=) :: PreOrd a => a -> a -> Bool
(>=) = geq
infix 4 >=

-- | Infix operator alias for 'leq'.
--
(≧) :: PreOrd a => a -> a -> Bool
(≧) = geq
infix 4 ≧

data LEQ
 = PEQ
 | PLT
 | PFalse
 deriving (Read, Show, Ord, Eq)

instance PreOrd LEQ

instance Semigroup LEQ where
  PEQ <> a = a
  a <> _ = a

instance Monoid LEQ where
  mempty = PEQ

boolLEQ :: LEQ -> Bool
boolLEQ PEQ = True
boolLEQ PLT = True
boolLEQ PFalse = False

leqLEQ :: (Eq a, PreOrd a) => a -> a -> LEQ
leqLEQ a b =
  if | a == b  -> PEQ
     | leq a b -> PLT
     | True    -> PFalse

class PreOrd1 f where
  liftLEQ :: (a -> b -> LEQ) -> f a -> f b -> LEQ

leq1 :: (PreOrd1 f, Eq a, PreOrd a) => f a -> f a -> Bool
leq1 a b = boolLEQ $ liftLEQ leqLEQ a b

instance PreOrd Void

instance PreOrd ()

instance PreOrd Bool

instance PreOrd Int
instance PreOrd Int8
instance PreOrd Int16
instance PreOrd Int32
instance PreOrd Int64
instance PreOrd Integer

instance PreOrd Word
instance PreOrd Word8
instance PreOrd Word16
instance PreOrd Word32
instance PreOrd Natural

instance PreOrd Char

instance PreOrd Ordering

instance PreOrd SomeTypeRep

instance PreOrd TyCon

instance PreOrd Version

instance PreOrd GeneralCategory

instance PreOrd Fingerprint

instance PreOrd IOMode

instance PreOrd SomeNat

instance PreOrd SomeSymbol

instance PreOrd IntPtr
instance PreOrd WordPtr
instance PreOrd CUIntPtr
instance PreOrd CIntPtr
instance PreOrd CPtrdiff
instance PreOrd (Ptr a)
instance PreOrd (FunPtr a)
instance PreOrd (ForeignPtr a)

instance PreOrd CUIntMax
instance PreOrd CIntMax

instance PreOrd CSUSeconds
instance PreOrd CUSeconds
instance PreOrd CTime
instance PreOrd CClock

instance PreOrd CSigAtomic
instance PreOrd CWchar

instance PreOrd CSize

instance PreOrd CBool

instance PreOrd CULLong
instance PreOrd CLLong
instance PreOrd CULong
instance PreOrd CLong
instance PreOrd CUInt
instance PreOrd CInt
instance PreOrd CUShort
instance PreOrd CShort
instance PreOrd CUChar
instance PreOrd CSChar
instance PreOrd CChar

instance PreOrd DecidedStrictness
instance PreOrd SourceStrictness
instance PreOrd SourceUnpackedness
instance PreOrd Associativity
instance PreOrd Fixity

instance PreOrd Monoid.Any
instance PreOrd Monoid.All

instance PreOrd ArithException
instance PreOrd ErrorCall
instance PreOrd ArrayException
instance PreOrd AsyncException

#if (MIN_VERSION_base(4,15,0))
instance PreOrd TimeoutKey
#endif

instance PreOrd SeekMode
instance PreOrd NewlineMode
instance PreOrd Newline
instance PreOrd BufferMode
instance PreOrd ExitCode

instance PreOrd Fd

#if (MIN_VERSION_base(4,14,0))
instance PreOrd CNfds
instance PreOrd CSocklen
#endif

#if defined(HTYPE_TIMER_T)
instance PreOrd CTimer
#endif

instance PreOrd CKey
instance PreOrd CId
instance PreOrd CFsFilCnt
instance PreOrd CFsBlkCnt
instance PreOrd CClockId
instance PreOrd CBlkSize
instance PreOrd CRLim
instance PreOrd CTcflag
instance PreOrd CSpeed
instance PreOrd CCc
instance PreOrd CUid
instance PreOrd CNlink
instance PreOrd CGid
instance PreOrd CSsize
instance PreOrd CPid
instance PreOrd COff
instance PreOrd CMode
instance PreOrd CIno
instance PreOrd CDev

instance PreOrd ThreadStatus
instance PreOrd BlockReason
instance PreOrd ThreadId

instance PreOrd ByteOrder

instance PreOrd Unique

instance (Eq a, PreOrd a) => PreOrd [a] where
  leq [] [] = True
  leq [] (_:_) = True
  leq (_:_) [] = False
  leq (x:xs) (y:ys) = case x == y of
                        True -> leq xs ys
                        False -> leq x y

instance PreOrd a => PreOrd (Maybe a) where
  leq Nothing _ = True
  leq _ Nothing = False
  leq (Just x) (Just y) = leq x y

instance Integral a => PreOrd (Ratio a)

instance PreOrd p => PreOrd (Par1 p) where
  leq (Par1 x) (Par1 y) = leq x y

-- TODO: as with list
instance (Eq a, PreOrd a) => PreOrd (NonEmpty a) where
  leq (x :| xs) (y :| ys) = case x == y of
                              True -> leq xs ys
                              False -> leq x y

--TODO: Does "instance PreOrd a => PreOrd (Down a)" exist properly?
deriving newtype instance PreOrd a => PreOrd (Monoid.Product a)
deriving newtype instance PreOrd a => PreOrd (Monoid.Sum a)
deriving newtype instance PreOrd a => PreOrd (Monoid.Dual a)
deriving newtype instance PreOrd a => PreOrd (Monoid.Last a)
deriving newtype instance PreOrd a => PreOrd (Monoid.First a)
deriving newtype instance PreOrd a => PreOrd (Functor.Identity a)
deriving newtype instance (Eq a, PreOrd a) => PreOrd (ZipList a)
deriving newtype instance PreOrd m => PreOrd (WrappedMonoid m)
deriving newtype instance PreOrd a => PreOrd (Last a)
deriving newtype instance PreOrd a => PreOrd (First a)
deriving newtype instance PreOrd a => PreOrd (Max a)
deriving newtype instance PreOrd a => PreOrd (Min a)
deriving newtype instance PreOrd a => PreOrd (Functor.Const a b)
deriving newtype instance PreOrd c => PreOrd (K1 i c p)

instance (PreOrd a, PreOrd b) => PreOrd (Either a b) where
  leq (Left _) (Right _) = True
  leq (Right _) (Left _) = False
  leq (Left x) (Left y) = leq x y
  leq (Right x) (Right y) = leq x y

instance PreOrd (V1 p)
instance PreOrd (U1 p)
instance PreOrd (TypeRep a)

instance (Ix i, PreOrd i, Eq e, PreOrd e) => PreOrd (Array i e) where
  leq arr1 arr2 = leq (assocs arr1) (assocs arr2)

instance PreOrd (Proxy s)

instance PreOrd a  => PreOrd (Arg a b) where
  leq (Arg x _) (Arg y _) = leq x y

instance PreOrd (Fixed a)
deriving newtype instance PreOrd (f p) => PreOrd (Rec1 f p)
instance PreOrd (URec Word p)
instance PreOrd (URec Int p)
instance PreOrd (URec Char p)
instance PreOrd (URec (Ptr ()) p)
instance PreOrd (a :~: b)
instance PreOrd (Coercion a b)
deriving newtype instance PreOrd (f a) => PreOrd (Monoid.Alt f a)
deriving newtype instance PreOrd (f a) => PreOrd (Monoid.Ap f a)

instance (Eq (f p), PreOrd (f p), (Eq (g p)), PreOrd (g p)) => PreOrd ((f :+: g) p) where
  leq (L1 _) (R1 _) = True
  leq (R1 _) (L1 _) = False
  leq (L1 x) (L1 y) = leq x y
  leq (R1 x) (R1 y) = leq x y

instance (Eq (f p), PreOrd (f p), PreOrd (g p)) => PreOrd ((f :*: g) p) where
  leq (x1 :*: y1) (x2 :*: y2) = case x1 == x2 of
                                  True -> leq y1 y2
                                  False -> leq x1 x2

instance PreOrd (a :~~: b)

instance (PreOrd1 f, PreOrd1 g, Eq a, PreOrd a) => PreOrd (Functor.Sum f g a) where
  leq = leq1

instance (PreOrd1 f, PreOrd1 g, Eq a, PreOrd a) => PreOrd (Functor.Product f g a) where
  leq = leq1

deriving newtype instance (PreOrd (f p)) => PreOrd (M1 i c f p)
deriving newtype instance (PreOrd (f (g p))) => PreOrd ((f :.: g) p)

instance (PreOrd1 f, PreOrd1 g, Eq a, PreOrd a) => PreOrd (Functor.Compose f g a) where
  leq = leq1

instance PreOrd1 IntMap.IntMap where
  liftLEQ cmp m n = liftLEQ cmp (toList m) (toList n)

instance (Eq v, PreOrd v) => PreOrd (IntMap.IntMap v) where
  leq m1 m2 = leq (toList m1) (toList m2)

instance PreOrd1 (Map.Map k) where
  liftLEQ cmp m n = liftLEQ cmp (toList m) (toList n)

instance (PreOrd k, Eq v, PreOrd v) => PreOrd (Map.Map k v) where
  leq m1 m2 = leq (toList m1) (toList m2)

instance PreOrd IntSet.IntSet

instance (Eq a, PreOrd a)  => PreOrd (Set.Set a) where
  leq s1 s2 = leq (toList s1) (toList s2)

instance (Eq a, PreOrd a, PreOrd b) => PreOrd (a, b) where
 leq (a1, b1) (a2, b2) = case a1 == a2 of
                           True -> leq b1 b2
                           False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, PreOrd c) => PreOrd (a, b, c) where
  leq (a1, b1, c1) (a2, b2, c2) =
    case a1 == a2 of
      True -> leq (b1, c1) (b2, c2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, PreOrd d) => PreOrd (a, b, c, d) where
  leq (a1, b1, c1, d1) (a2, b2, c2, d2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1) (b2, c2, d2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, PreOrd e) => PreOrd (a, b, c, d, e) where
  leq (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1) (b2, c2, d2, e2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, PreOrd f) => PreOrd (a, b, c, d, e, f) where
  leq (a1, b1, c1, d1, e1, f1) (a2, b2, c2, d2, e2, f2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1) (b2, c2, d2, e2, f2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, PreOrd g) =>
         PreOrd (a, b, c, d, e, f, g) where
  leq (a1, b1, c1, d1, e1, f1, g1) (a2, b2, c2, d2, e2, f2, g2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1) (b2, c2, d2, e2, f2, g2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, PreOrd h) =>
         PreOrd (a, b, c, d, e, f, g, h) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1) (a2, b2, c2, d2, e2, f2, g2, h2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1) (b2, c2, d2, e2, f2, g2, h2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, PreOrd i) =>
         PreOrd (a, b, c, d, e, f, g, h, i) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1) (a2, b2, c2, d2, e2, f2, g2, h2, i2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1) (b2, c2, d2, e2, f2, g2, h2, i2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, PreOrd j) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1) (b2, c2, d2, e2, f2, g2, h2, i2, j2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, Eq j, PreOrd j, PreOrd k) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j, k) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1, k1) (b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, Eq j, PreOrd j, Eq k, PreOrd k, PreOrd l) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j, k, l) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1) (b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, Eq j, PreOrd j, Eq k, PreOrd k, Eq l, PreOrd l, PreOrd m) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1) (b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
      False -> leq a1 a2

instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, Eq j, PreOrd j, Eq k, PreOrd k, Eq l, PreOrd l, Eq m, PreOrd m, PreOrd n) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1) (b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
      False -> leq a1 a2


instance (Eq a, PreOrd a, Eq b, PreOrd b, Eq c, PreOrd c, Eq d, PreOrd d, Eq e, PreOrd e, Eq f, PreOrd f, Eq g, PreOrd g, Eq h, PreOrd h, Eq i, PreOrd i, Eq j, PreOrd j, Eq k, PreOrd k, Eq l, PreOrd l, Eq m, PreOrd m, Eq n, PreOrd n, PreOrd o) =>
         PreOrd (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  leq (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2) =
    case a1 == a2 of
      True -> leq (b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1) (b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
      False -> leq a1 a2

instance PreOrd1 [] where
  liftLEQ _ [] [] = PEQ
  liftLEQ _ [] (_:_) = PLT
  liftLEQ _ (_:_) [] = PFalse
  liftLEQ cmp (x:xs) (y:ys) =
    cmp x y <> liftLEQ cmp xs ys

instance PreOrd1 Maybe where
  liftLEQ _ Nothing Nothing = PEQ
  liftLEQ _ Nothing (Just _) = PLT
  liftLEQ _ (Just _) Nothing = PFalse
  liftLEQ cmp (Just x) (Just y) = cmp x y

#if (MIN_VERSION_base(4,15,0))
instance PreOrd1 Solo where
  liftLEQ cmp (Solo a) (Solo b) = cmp a b
#endif

instance PreOrd1 NonEmpty where
  liftLEQ cmp (x :| xs) (y :| ys) = cmp x y <> liftLEQ cmp xs ys

instance PreOrd1 Down where
  liftLEQ cmp (Down x) (Down y) = cmp x y

instance PreOrd1 Functor.Identity where
  liftLEQ cmp (Functor.Identity x) (Functor.Identity y) = cmp x y

instance (Eq a, PreOrd a) => PreOrd1 ((,) a) where
  liftLEQ cmp (a1, b1) (a2, b2) =
    if a1 == a2
    then cmp b1 b2
    else leqLEQ a1 a2

instance PreOrd1 Proxy where
  liftLEQ _ _ _ = PEQ

instance (PreOrd1 f, PreOrd1 g) => PreOrd1 (Functor.Sum f g) where
  liftLEQ cmp (Functor.InL x1) (Functor.InL x2) = liftLEQ cmp x1 x2
  liftLEQ _   (Functor.InL _) (Functor.InR _) = PLT
  liftLEQ _   (Functor.InR _) (Functor.InL _) = PFalse
  liftLEQ cmp (Functor.InR y1) (Functor.InR y2) = liftLEQ cmp y1 y2

instance (PreOrd1 f, PreOrd1 g) => PreOrd1 (Functor.Product f g) where
  liftLEQ cmp (Functor.Pair x1 y1) (Functor.Pair x2 y2) =
    liftLEQ cmp x1 x2 <> liftLEQ cmp y1 y2

instance (PreOrd1 f, PreOrd1 g) => PreOrd1 (Functor.Compose f g) where
  liftLEQ cmp (Functor.Compose x) (Functor.Compose y) =
    liftLEQ (liftLEQ cmp) x y

{- These instances maybe want PreOrd2?

instance PreOrd a => PreOrd1 (Functor.Const a) where
instance (Eq a, PreOrd a) => PreOrd1 (Either a) where
  liftLEQ _ (Left _) (Right _) = PLT
  liftLEQ _ (Right _) (Left _) = PFalse
  liftLEQ _ (Left x) (Left y) = leqLEQ x y
  liftLEQ cmp (Right x) (Right y) = cmp x y

-}
