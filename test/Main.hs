{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Control.Monad (join)
import Dahdit (Binary, StaticByteSized, ViaGeneric (..), ViaStaticGeneric (..), Word16LE)
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.Dahdit.Arb (Arb (..), ArbGeneric (..), DahditIdx)
import Test.Dahdit.Tasty (FileExpect (..), UnitExpect (..), dahditMain, fileRT, genRT, staticRT, testRT, unitRT)
import Test.Falsify.Generator (Gen)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

data P

type I = DahditIdx P

proxyI :: Proxy a -> Proxy (I, a)
proxyI _ = Proxy

arbI :: Arb I a => Proxy a -> Gen a
arbI = arb (Proxy @I)

data DynFoo = DynFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (Binary) via (ViaGeneric DynFoo)
  deriving (Arb I) via (ArbGeneric I DynFoo)

data StaFoo = StaFoo !Word8 !Word16LE
  deriving stock (Eq, Show, Generic)
  deriving (StaticByteSized, Binary) via (ViaStaticGeneric StaFoo)
  deriving (Arb I) via (ArbGeneric I StaFoo)

testDummy :: TestTree
testDummy = testCase "dummy" $ do
  let actual = (1 + 1) :: Int
      expected = 2 :: Int
  actual @?= expected

main :: IO ()
main =
  dahditMain
    $ testGroup
      "DahditTest"
    $ testRT
      <$> join
        [
          [ genRT "DynFoo prop" (arbI (Proxy @DynFoo))
          , unitRT "DynFoo unit" (DynFoo 1 2) (UnitExpectBytes [1, 2, 0])
          , fileRT "DynFoo file" "testdata/dynfoo.bin" (FileExpectValue (DynFoo 1 2))
          ]
        , let p = Proxy @StaFoo
          in  staticRT p
                <$> [ genRT "StaFoo prop" (arbI p)
                    , unitRT "StaFoo unit" (StaFoo 3 4) (UnitExpectText "\ETX\EOT\NUL")
                    , fileRT "DynFoo file" "testdata/stafoo.bin" (FileExpectValue (StaFoo 1 2))
                    ]
        ]
