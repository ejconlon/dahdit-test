module Test.Dahdit.Tasty
  ( GenRTCase (..)
  , dynGenRTCase
  , staGenRTCase
  , testGenRTCase
  , FileRTCase (..)
  , testFileRTCase
  , UnitRTCase (..)
  , testUnitRTCase
  , RTCase (..)
  , testRTCase
  )
where

import Control.Monad (unless, when)
import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized (..), decodeEnd, decodeFileEnd, encode)
import Data.ByteString.Short qualified as BSS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Test.Falsify.Generator (Gen)
import Test.Falsify.Predicate qualified as FR
import Test.Falsify.Property (Property)
import Test.Falsify.Property qualified as FP
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

failAt :: MonadFail m => TestName -> GetError -> ByteCount -> m ()
failAt name err bc = fail ("Decode " ++ name ++ " failed at " ++ show (unByteCount bc) ++ ": " ++ show err)

-- | A "round-trip" case
data GenRTCase where
  GenRTCase :: (Eq a, Show a, Binary a) => TestName -> Gen a -> Maybe ByteCount -> GenRTCase

-- | A "dynamically-sized" case
dynGenRTCase :: (Eq a, Show a, Binary a) => TestName -> Gen a -> GenRTCase
dynGenRTCase name gen = GenRTCase name gen Nothing

proxyFor :: f a -> Proxy a
proxyFor _ = Proxy

-- | A "statically-sized" case
staGenRTCase :: (Eq a, Show a, StaticByteSized a, Binary a) => TestName -> Gen a -> GenRTCase
staGenRTCase name gen = GenRTCase name gen (Just (staticByteSize (proxyFor gen)))

type AssertEq m = forall a. (Eq a, Show a) => a -> a -> m ()

propAssertEq :: AssertEq Property
propAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))

runValueRTCase :: (MonadFail m, Eq a, Show a, Binary a) => AssertEq m -> TestName -> Maybe ByteCount -> a -> m ()
runValueRTCase assertEq name mayStaBc startVal = do
  let startDynBc = byteSize startVal
  for_ mayStaBc (assertEq startDynBc)
  let encVal = encode startVal
      encBc = ByteCount (BSS.length encVal)
  let (endRes, endConBc) = decodeEnd encVal
  case endRes of
    Left err -> failAt name err endConBc
    Right endVal -> do
      assertEq endVal startVal
      let endDynBc = byteSize endVal
      assertEq endDynBc startDynBc
      assertEq endConBc startDynBc
      assertEq encBc startDynBc

testGenRTCase :: GenRTCase -> TestTree
testGenRTCase (GenRTCase name gen mayStaBc) =
  testProperty name $
    FP.gen gen >>= runValueRTCase propAssertEq name mayStaBc

data FileExpect a
  = FileExpectOk
  | FileExpectSpecific !a
  | FileExpectFail
  deriving stock (Eq, Ord, Show)

data FileRTCase where
  FileRTCase :: (Eq a, Show a, Binary a) => TestName -> FilePath -> FileExpect a -> Maybe ByteCount -> FileRTCase

proxyForFE :: FileExpect a -> Proxy a
proxyForFE _ = Proxy

decodeFileAs :: Binary a => Proxy a -> FilePath -> IO (Either GetError a, ByteCount)
decodeFileAs _ = decodeFileEnd

shouldFail :: FileExpect a -> Bool
shouldFail = \case
  FileExpectFail -> True
  _ -> False

reDecodeEnd :: Binary a => a -> (Either GetError a, ByteCount)
reDecodeEnd = decodeEnd . encode @_ @BSS.ShortByteString

testFileRTCase :: FileRTCase -> TestTree
testFileRTCase (FileRTCase name fn fe mayStaBc) = testCase name $ do
  (fileRes, fileBc) <- decodeFileAs (proxyForFE fe) fn
  case fileRes of
    Left err ->
      unless
        (shouldFail fe)
        (fail ("Decode " ++ fn ++ " failed at " ++ show (unByteCount fileBc) ++ ": " ++ show err))
    Right fileVal -> do
      when (shouldFail fe) (fail "Expected failure")
      -- Rendered size should not be larger than input size
      let dynBc = byteSize fileVal
      for_ mayStaBc (dynBc @?=)
      unless (dynBc <= fileBc) (fail "Bad byte size")
      let (endRes, endConBc) = reDecodeEnd fileVal
      case endRes of
        Left err -> fail ("Re-decode " ++ fn ++ " failed: " ++ show err)
        Right endVal -> do
          endVal @?= fileVal
          endConBc @?= dynBc

data UnitRTCase where
  UnitRTCase :: (Eq a, Show a, Binary a) => TestName -> a -> Maybe ByteCount -> UnitRTCase

testUnitRTCase :: UnitRTCase -> TestTree
testUnitRTCase (UnitRTCase name val mayStaBc) =
  testCase name $
    runValueRTCase (@?=) name mayStaBc val

data RTCase
  = RTCaseGen !GenRTCase
  | RTCaseFile !FileRTCase
  | RTCaseUnit !UnitRTCase

testRTCase :: RTCase -> TestTree
testRTCase = \case
  RTCaseGen x -> testGenRTCase x
  RTCaseFile x -> testFileRTCase x
  RTCaseUnit x -> testUnitRTCase x
