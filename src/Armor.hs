{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Armor
  ( Version(..)
  , Serialization
  , Armored(..)
  , ArmorConfig(..)
  , defArmorConfig
  , testArmor
  , testArmorMany
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Typeable
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
-- | Version has a Show instance so you can use numeric literals.
instance Num (Version a) where
    Version a + Version b = Version (a+b)
    Version a - Version b = Version (a-b)
    Version a * Version b = Version (a*b)
    negate (Version a) = Version (negate a)
    abs (Version a) = Version (abs a)
    signum (Version a) = Version (signum a)
    fromInteger i = Version (fromInteger i)


------------------------------------------------------------------------------
-- | A serialization is a tuple of @(a -> ByteString)@ and @(ByteString ->
-- Maybe a)@.  Represented here as a prism.
type Serialization a = APrism' ByteString a


------------------------------------------------------------------------------
-- | Core type class for armoring types.  Includes a version and all the
-- type's serializations that you want to armor.
class Armored a where
  -- | Current version number for the data type.
  version :: Version a
  -- | Map of serializations keyed by a unique ID used to refer to each
  -- serialization.
  serializations :: Map String (Serialization a)


------------------------------------------------------------------------------
-- | Config data for armor tests.
data ArmorConfig = ArmorConfig
    { acStoreDir    :: FilePath
    -- ^ Directory where all the test serializations are stored.
    , acNumVersions :: Word
    -- ^ How many versions back to test for backwards compatibility.  A value
    -- of 0 means that it only tests that the current version satisfies
    -- @parse . render == id@.  1 means that it will verify that the previous
    -- version can still be parse.  2 the previous two versions, etc.
    }


------------------------------------------------------------------------------
-- | Default value for ArmorConfig.
defArmorConfig :: ArmorConfig
defArmorConfig = ArmorConfig "test-data" 1


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
    testIt s = test (testSerialization ac valId val s)


------------------------------------------------------------------------------
-- Same as 'testArmor', but more convenient for testing several values of the
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
testSerialization
    :: forall a. (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> String
    -> a
    -> (String, APrism' ByteString a)
    -> Assertion
testSerialization ac valId val s@(_,p) = do
    let d = getVersionDir ac val s
        f = getVersionFilename valId curVer
        fp = d </> f
    createDirectoryIfMissing True d
    fileExists <- doesFileExist fp
    when (not fileExists) $
      B.writeFile fp (review (clonePrism p) val)
    mapM_ (assertVersionParses d . Version) vs
  where
    curVer :: Version a
    curVer = version
    vs = reverse [unVersion curVer - acNumVersions ac .. unVersion curVer]
    assertVersionParses d ver = do
        let f = getVersionFilename valId ver
            fp = d </> f
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
getVersionFilename :: String -> Version a -> String
getVersionFilename valId ver = printf "%s-%03d.test" valId (unVersion ver)


------------------------------------------------------------------------------
getVersionDir :: Typeable a => ArmorConfig -> a -> (FilePath, t) -> FilePath
getVersionDir ac val (nm,_) = acStoreDir ac </> show (typeOf val) </> nm
