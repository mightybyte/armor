{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Armor
  ( Version(..)
  , Armored(..)
  , ArmorMode(..)
  , ArmorConfig(..)
  , defArmorConfig
  , testArmor
  , testArmorMany
  , testSerialization
  , GoldenTest(..)
  , goldenFilePath
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Char
import           Data.Hashable
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Typeable
#if !MIN_VERSION_base(4,8,0)
import           Data.Word
#endif
import           Numeric
import           System.Directory
import           System.FilePath
import           Test.HUnit.Base
import           Text.Printf
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Version numbers are simple monotonically increasing positive integers.
newtype Version a = Version { unVersion :: Word }
  deriving (Eq,Ord,Show,Read)


------------------------------------------------------------------------------
-- | Core type class for armoring types.  Includes a version and all the
-- type's serializations that you want to armor.
class Armored a where
  -- | Current version number for the data type.
  version :: Version a
  -- | Map of serializations keyed by a unique ID used to refer to each
  -- serialization. A serialization is a tuple of @(a -> ByteString)@ and
  -- @(ByteString -> Maybe a)@. Represented here as a prism.
  serializations :: Map String (APrism' ByteString a)


------------------------------------------------------------------------------
-- | The mode of operation for armor test cases.
data ArmorMode
  = SaveOnly
    -- ^ Write test files for serializations that don't have them, but don't
    -- do any tests to verify that existing files are deserialized properly.
  | TestOnly
    -- ^ Run tests to verify that existing files are deserialized properly,
    -- but don't write any missing files.
  | SaveAndTest
    -- ^ Do both the save and test phases.
  deriving (Eq,Ord,Show,Read,Enum,Bounded)


------------------------------------------------------------------------------
-- | Config data for armor tests.
data ArmorConfig = ArmorConfig
    { acArmorMode   :: ArmorMode
    , acStoreDir    :: FilePath
    -- ^ Directory where all the test serializations are stored.
    , acNumVersions :: Maybe Word
    -- ^ How many versions back to test for backwards compatibility.  A value
    -- of @Just 0@ means that it only tests that the current version satisfies
    -- @parse . render == id@.  @Just 1@ means that it will verify that the
    -- previous version can still be parse.  @Just 2@ the previous two
    -- versions, etc.  Nothing means that all versions will be tested.
    }


------------------------------------------------------------------------------
-- | Default value for ArmorConfig.
defArmorConfig :: ArmorConfig
defArmorConfig = ArmorConfig SaveAndTest "test-data" Nothing


------------------------------------------------------------------------------
-- | Tests the serialization backwards compatibility of a data type by storing
-- serialized representations in .test files to be checked into your project's
-- version control.
--
-- First, this function checks the directory 'acStoreDir' for the existence of
-- a file @foo-000.test@.  If it doesn't exist, it creates it for each
-- serialization with the serialized representation of the val parameter.
--
-- Next, it checks that the serialized formats in the most recent
-- 'acNumVersions' of the stored @.test@ files are parsable by the current
-- version of the serialization.
testArmor
    :: (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> String
    -> a
    -> Test
testArmor ac valId val =
    TestList [ testIt s | s <- M.toList serializations ]
  where
    testIt s = test (testSerialization ac goldenFilePath valId s val)


------------------------------------------------------------------------------
-- | Same as 'testArmor', but more convenient for testing several values of the
-- same type.
testArmorMany
    :: (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> Map String a
    -> Test
testArmorMany ac valMap = TestList $ map doOne $ M.toList valMap
  where
    doOne (k,v) = TestLabel k $ testArmor ac k v


------------------------------------------------------------------------------
-- | Lower level assertion function that works for a wider array of test
-- frameworks.
testSerialization
    :: forall a. (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> (GoldenTest a -> FilePath)
    -- ^ Customizable location where the serializations will be stored. We
    -- recommend 'goldenFilePath' as a standard out-of-the-box scheme.
    -> String
    -> (String, APrism' ByteString a)
    -> a
    -> Assertion
testSerialization ac makeFilePath valName (sname,p) val = do
    ensureTestFileExists
    when (acArmorMode ac /= SaveOnly) $
      mapM_ (assertVersionParses . Version) vs
  where
    makeGT = GoldenTest val valName sname p
    curVer :: Version a
    curVer = version
    vs = reverse [maybe 0 (unVersion curVer -) (acNumVersions ac) .. unVersion curVer]
    ensureTestFileExists = do
      let fp = acStoreDir ac </> makeFilePath (makeGT curVer)
          d = dropFileName fp
      when (acArmorMode ac /= TestOnly) $ do
        createDirectoryIfMissing True d
        fileExists <- doesFileExist fp
        when (not fileExists) $
          B.writeFile fp (review (clonePrism p) val)
    assertVersionParses ver = do
        let fp = acStoreDir ac </> makeFilePath (makeGT ver)
        exists <- doesFileExist fp
        if exists
          then do bs <- B.readFile fp
                  case preview (clonePrism p) bs of
                    Nothing -> assertFailure $
                      printf "Not backwards compatible with version %d: %s"
                             (unVersion ver) fp
                    Just v -> assertEqual ("File parsed but values didn't match: " ++ fp) val v
          else putStrLn $ "\nSkipping missing file " ++ fp

------------------------------------------------------------------------------
-- | Data structure that holds all the values needed for a golden test
data GoldenTest a = GoldenTest
  { gtTestVal :: a
  , gtValName :: String
  , gtSerializationName :: String
  , gtPrism :: APrism' ByteString a
  , gtVersion :: Version a
  }

------------------------------------------------------------------------------
-- | Constructs the FilePath where the serialization will be stored (relative to
-- the base directory defined in ArmorConfig).
--
-- This function uses typeOf as a part of the directory hierarchy to
-- disambiguate tests for different data types. typeOf can contain single
-- quotes, spaces, and parenthesis in the case of type constructors that have
-- type variables so we only take the first alphanumeric characters so that the
-- paths will be meaningful to humans and then add four characters of the type's
-- hash for disambiguation.
goldenFilePath :: Typeable a => GoldenTest a -> FilePath
goldenFilePath gt =
    (concat [takeWhile isAlpha ty, "-", h]) </>
    gtSerializationName gt </>
    printf "%s-%03d.test" (gtValName gt) (unVersion $ gtVersion gt)
  where
    ty = show $ typeOf $ gtTestVal gt
    h = take 4 $ showHex (abs $ hash ty) ""
