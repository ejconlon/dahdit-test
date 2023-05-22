{-# LANGUAGE UndecidableInstances #-}

module Test.Dahdit.Arb
  ( genSigned
  , genUnsigned
  , genFractional
  , genEnum
  , genSum
  , genList
  , genSeq
  , genString
  , genSBS
  , Arb (..)
  , ArbSigned (..)
  , ArbUnsigned (..)
  , ArbFractional (..)
  , ArbEnum (..)
  , ArbGeneric (..)
  , DahditIdx
  )
where

import Control.Applicative (liftA2)
import Dahdit
  ( DoubleBE (..)
  , DoubleLE (..)
  , FloatBE (..)
  , FloatLE (..)
  , Int16BE (..)
  , Int16LE (..)
  , Int32BE (..)
  , Int32LE (..)
  , Int64BE (..)
  , Int64LE (..)
  , Word16BE (..)
  , Word16LE (..)
  , Word32BE (..)
  , Word32LE (..)
  , Word64BE (..)
  , Word64LE (..)
  )
import Data.Bits (FiniteBits (..))
import Data.ByteString.Internal (w2c)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as BSS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic (..), K1 (..), M1 (..), U1 (..), (:*:) (..), (:+:) (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Generator qualified as FG
import Test.Falsify.Range qualified as FR

genSigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genSigned = FG.integral (FR.withOrigin (minBound, maxBound) 0)

genUnsigned :: (Integral a, FiniteBits a, Bounded a) => Gen a
genUnsigned = FG.integral (FR.between (0, maxBound))

genFractional :: Fractional a => Gen a
genFractional = do
  -- Picked so bound**2 fits in int
  let bound = 3037000499 :: Int
  n <- FG.integral (FR.between (0, bound))
  b <- FG.integral (FR.between (1, bound))
  a <- FG.integral (FR.withOrigin ((-n) * b, n * b) 0)
  return (fromRational (fromIntegral a % fromIntegral b))

genEnum :: (Enum a, Bounded a) => Gen a
genEnum = let b = minBound in FG.elem (b :| drop 1 [b .. maxBound])

genSum :: NonEmpty (Gen a) -> Gen a
genSum (g :| gs) = foldr FG.choose g gs

genList :: Word -> Word -> Gen a -> Gen [a]
genList mn mx = FG.list (FR.between (mn, mx))

genSeq :: Word -> Word -> Gen a -> Gen (Seq a)
genSeq mn mx = fmap Seq.fromList . genList mn mx

genString :: Word -> Word -> Gen String
genString mn mx = genList mn mx (fmap w2c genUnsigned)

genSBS :: Word -> Word -> Gen ShortByteString
genSBS mn mx = fmap BSS.pack (genList mn mx genUnsigned)

class Arb p a where
  arb :: Proxy p -> Gen a

newtype ArbSigned a = ArbSigned {unArbSigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb p (ArbSigned a) where
  arb _ = fmap ArbSigned genSigned

newtype ArbUnsigned a = ArbUnsigned {unArbUnsigned :: a}

instance (Integral a, FiniteBits a, Bounded a) => Arb p (ArbUnsigned a) where
  arb _ = fmap ArbUnsigned genUnsigned

newtype ArbFractional a = ArbFractional {unArbFractional :: a}

instance (Fractional a) => Arb p (ArbFractional a) where
  arb _ = fmap ArbFractional genFractional

newtype ArbEnum a = ArbEnum {unArbEnum :: a}

instance (Enum a, Bounded a) => Arb p (ArbEnum a) where
  arb _ = fmap ArbEnum genEnum

class GArb p f where
  garb :: Proxy p -> Gen (f a)

-- Unit
instance GArb p U1 where
  garb _ = pure U1

-- Metadata
instance GArb p a => GArb p (M1 i c a) where
  garb = fmap M1 . garb

-- Product
instance (GArb p a, GArb p b) => GArb p (a :*: b) where
  garb p = liftA2 (:*:) (garb p) (garb p)

-- Sum
instance (GArb p a, GArb p b) => GArb p (a :+: b) where
  garb p = FG.choose (fmap L1 (garb p)) (fmap R1 (garb p))

-- Field
instance Arb p a => GArb p (K1 i a) where
  garb = fmap K1 . arb

newtype ArbGeneric p a = ArbGeneric {unArbGeneric :: a}

instance (Generic t, GArb p (Rep t)) => Arb p (ArbGeneric p t) where
  arb = fmap (ArbGeneric . to) . garb

data DahditIdx a

deriving via (ArbUnsigned Word8) instance Arb (DahditIdx p) Word8

deriving via (ArbSigned Int8) instance Arb (DahditIdx p) Int8

deriving via (ArbUnsigned Word16) instance Arb (DahditIdx p) Word16

deriving via (ArbSigned Int16) instance Arb (DahditIdx p) Int16

deriving via (ArbUnsigned Word32) instance Arb (DahditIdx p) Word32

deriving via (ArbSigned Int32) instance Arb (DahditIdx p) Int32

deriving via (ArbUnsigned Word64) instance Arb (DahditIdx p) Word64

deriving via (ArbSigned Int64) instance Arb (DahditIdx p) Int64

deriving via (ArbFractional Float) instance Arb (DahditIdx p) Float

deriving via (ArbFractional Double) instance Arb (DahditIdx p) Double

deriving via (ArbSigned Int) instance Arb (DahditIdx p) Int

deriving newtype instance Arb (DahditIdx p) Word16LE

deriving newtype instance Arb (DahditIdx p) Int16LE

deriving newtype instance Arb (DahditIdx p) Word32LE

deriving newtype instance Arb (DahditIdx p) Int32LE

deriving newtype instance Arb (DahditIdx p) Word64LE

deriving newtype instance Arb (DahditIdx p) Int64LE

deriving newtype instance Arb (DahditIdx p) FloatLE

deriving newtype instance Arb (DahditIdx p) DoubleLE

deriving newtype instance Arb (DahditIdx p) Word16BE

deriving newtype instance Arb (DahditIdx p) Int16BE

deriving newtype instance Arb (DahditIdx p) Word32BE

deriving newtype instance Arb (DahditIdx p) Int32BE

deriving newtype instance Arb (DahditIdx p) Word64BE

deriving newtype instance Arb (DahditIdx p) Int64BE

deriving newtype instance Arb (DahditIdx p) FloatBE

deriving newtype instance Arb (DahditIdx p) DoubleBE

instance Arb (DahditIdx p) Char where
  arb = fmap w2c . arb

deriving via
  (ArbGeneric (DahditIdx p) ())
  instance
    Arb (DahditIdx p) ()

deriving via
  (ArbGeneric (DahditIdx p) Bool)
  instance
    Arb (DahditIdx p) Bool

deriving via
  (ArbGeneric (DahditIdx p) (Maybe a))
  instance
    Arb (DahditIdx p) a => Arb (DahditIdx p) (Maybe a)

deriving via
  (ArbGeneric (DahditIdx p) (Either a b))
  instance
    (Arb (DahditIdx p) a, Arb (DahditIdx p) b) => Arb (DahditIdx p) (Either a b)

deriving via
  (ArbGeneric (DahditIdx p) (a, b))
  instance
    (Arb (DahditIdx p) a, Arb (DahditIdx p) b) => Arb (DahditIdx p) (a, b)

deriving via
  (ArbGeneric (DahditIdx p) (a, b, c))
  instance
    (Arb (DahditIdx p) a, Arb (DahditIdx p) b, Arb (DahditIdx p) c) => Arb (DahditIdx p) (a, b, c)

deriving via
  (ArbGeneric (DahditIdx p) (a, b, c, d))
  instance
    (Arb (DahditIdx p) a, Arb (DahditIdx p) b, Arb (DahditIdx p) c, Arb (DahditIdx p) d) => Arb (DahditIdx p) (a, b, c, d)

deriving via
  (ArbGeneric (DahditIdx p) (a, b, c, d, e))
  instance
    (Arb (DahditIdx p) a, Arb (DahditIdx p) b, Arb (DahditIdx p) c, Arb (DahditIdx p) d, Arb (DahditIdx p) e) => Arb (DahditIdx p) (a, b, c, d, e)
