module Test.Dahdit.Tasty
  ( RT
  , genRT
  , fileRT
  , unitRT
  , testRT
  , staticRT
  , DahditWriteMissing (..)
  )
where

import Control.Monad (unless, when)
import Dahdit (Binary (..), ByteCount (..), GetError, StaticByteSized (..), decodeEnd, decodeFileEnd, encode, encodeFile)
import Data.ByteString.Short qualified as BSS
import Data.Foldable (for_)
import Data.Proxy (Proxy (..))
import Data.Tagged (Tagged, untag)
import Options.Applicative (help, long, switch)
import System.Directory (doesFileExist)
import Test.Falsify.Generator (Gen)
import Test.Falsify.Predicate qualified as FR
import Test.Falsify.Property (Property)
import Test.Falsify.Property qualified as FP
import Test.Tasty (TestName, TestTree, askOption)
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Options (IsOption (..), safeRead)

failAt :: MonadFail m => TestName -> GetError -> ByteCount -> m ()
failAt name err bc = fail ("Decode " ++ name ++ " failed at " ++ show (unByteCount bc) ++ ": " ++ show err)

-- | A "round-trip" case
data GenRT where
  GenRT :: (Eq a, Show a, Binary a) => TestName -> Gen a -> Maybe ByteCount -> GenRT

genRT :: (Eq a, Show a, Binary a) => TestName -> Gen a -> RT
genRT name gen = RTGen (GenRT name gen Nothing)

type AssertEq m = forall a. (Eq a, Show a) => a -> a -> m ()

propAssertEq :: AssertEq Property
propAssertEq x y = FP.assert (FR.eq FR..$ ("LHS", x) FR..$ ("RHS", y))

runValueRT :: (MonadFail m, Eq a, Show a, Binary a) => AssertEq m -> TestName -> Maybe ByteCount -> a -> m ()
runValueRT assertEq name mayStaBc startVal = do
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

testGenRT :: GenRT -> TestTree
testGenRT (GenRT name gen mayStaBc) =
  testProperty name $
    FP.gen gen >>= runValueRT propAssertEq name mayStaBc

data FileExpect a
  = FileExpectOk
  | FileExpectSpecific !a
  | FileExpectFail
  deriving stock (Eq, Ord, Show)

data FileRT where
  FileRT :: (Eq a, Show a, Binary a) => TestName -> FilePath -> FileExpect a -> Maybe ByteCount -> FileRT

fileRT :: (Eq a, Show a, Binary a) => TestName -> FilePath -> FileExpect a -> RT
fileRT name fn ex = RTFile (FileRT name fn ex Nothing)

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

testFileRT :: FileRT -> TestTree
testFileRT (FileRT name fn fe mayStaBc) = askOption $ \(DahditWriteMissing wm) ->
  testCase name $ do
    exists <- doesFileExist fn
    if exists
      then do
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
      else case (wm, fe) of
        (True, FileExpectSpecific val) -> encodeFile val fn
        _ -> fail ("Missing file: " ++ fn)

data UnitRT where
  UnitRT :: (Eq a, Show a, Binary a) => TestName -> a -> Maybe ByteCount -> UnitRT

unitRT :: (Eq a, Show a, Binary a) => TestName -> a -> RT
unitRT name val = RTUnit (UnitRT name val Nothing)

testUnitRT :: UnitRT -> TestTree
testUnitRT (UnitRT name val mayStaBc) =
  testCase name $
    runValueRT (@?=) name mayStaBc val

data RT
  = RTGen !GenRT
  | RTFile !FileRT
  | RTUnit !UnitRT

testRT :: RT -> TestTree
testRT = \case
  RTGen x -> testGenRT x
  RTFile x -> testFileRT x
  RTUnit x -> testUnitRT x

staticRT :: StaticByteSized a => Proxy a -> RT -> RT
staticRT p = go
 where
  sz = Just (staticByteSize p)
  go = \case
    RTGen (GenRT x y _) -> RTGen (GenRT x y sz)
    RTFile (FileRT x y z _) -> RTFile (FileRT x y z sz)
    RTUnit (UnitRT x y _) -> RTUnit (UnitRT x y sz)

newtype DahditWriteMissing = DahditWriteMissing {unDahditWriteMissing :: Bool}
  deriving stock (Show)
  deriving newtype (Eq, Ord)

instance IsOption DahditWriteMissing where
  defaultValue = DahditWriteMissing False
  parseValue = fmap DahditWriteMissing . safeRead
  optionName = return "dahdit-write-missing"
  optionHelp = return "Write missing test files"
  optionCLParser =
    DahditWriteMissing
      <$> switch
        ( long (untag (optionName :: Tagged DahditWriteMissing String))
            <> help (untag (optionHelp :: Tagged DahditWriteMissing String))
        )
