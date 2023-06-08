{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( main
  )
where

import Control.Monad (join)
import Dahdit (Binary, ByteCount (..), GetError, StaticByteSized (..), ViaGeneric (..), ViaStaticGeneric (..), Word16LE, decodeEnd, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.Dahdit.Arb (Arb (..), ArbGeneric (..), DahditIdx)
import Test.Daytripper (Expect, MonadExpect (..), daytripperMain, expectDuring, mkExpect, mkFileRT, mkPropRT, mkUnitRT, testRT)
import Test.Falsify.Generator (Gen)
import Test.Tasty (testGroup)

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

expectDD :: (MonadExpect m, Binary a, Eq a, Show a) => Expect m a ByteString (Either GetError a)
expectDD = mkExpect enc dec
 where
  enc = expectLiftIO . encode
  dec = expectLiftIO . fmap fst . decodeEnd

expectBytes :: MonadExpect m => [Word8] -> Expect m a ByteString c -> Expect m a ByteString c
expectBytes ws = expectDuring (\_ bs -> expectAssertEq bs (BS.pack ws))

proxyBefore :: Expect m a b c -> Proxy a
proxyBefore _ = Proxy

expectStatic :: (MonadExpect m, StaticByteSized a) => Expect m a ByteString c -> Expect m a ByteString c
expectStatic ex =
  let len = unByteCount (staticByteSize (proxyBefore ex))
  in  expectDuring (\_ bs -> expectAssertEq (BS.length bs) len) ex

main :: IO ()
main =
  daytripperMain $
    testGroup "DahditTest" $
      testRT
        <$> join
          [
            [ mkPropRT "DynFoo prop" expectDD (arbI (Proxy @DynFoo))
            , mkUnitRT "DynFoo unit" (expectBytes [1, 2, 0] expectDD) (DynFoo 1 2)
            , mkFileRT "DynFoo file" expectDD "testdata/dynfoo.bin" (DynFoo 1 2)
            ]
            --     , let p = Proxy @StaFoo
            --       in  staticRT p
            --             <$> [ genRT "StaFoo prop" (arbI p)
            --                 , unitRT "StaFoo unit" (StaFoo 3 4) (UnitExpectText "\ETX\EOT\NUL")
            --                 , fileRT "DynFoo file" "testdata/stafoo.bin" (FileExpectValue (StaFoo 1 2))
            --                 ]
          ]
