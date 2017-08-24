{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Armor where

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
newtype Version a = Version { unVersion :: Word }
  deriving (Eq,Ord,Show,Read)


------------------------------------------------------------------------------
instance Num (Version a) where
    Version a + Version b = Version (a+b)
    Version a - Version b = Version (a-b)
    Version a * Version b = Version (a*b)
    negate (Version a) = Version (negate a)
    abs (Version a) = Version (abs a)
    signum (Version a) = Version (signum a)
    fromInteger i = Version (fromInteger i)


------------------------------------------------------------------------------
type Serialization a = APrism' ByteString a


------------------------------------------------------------------------------
class Armored a where
  version :: Version a
  serializations :: Map String (Serialization a)


------------------------------------------------------------------------------
data ArmorConfig = ArmorConfig
    { ccStoreDir    :: FilePath
    , ccNumVersions :: Word
    }


defConfig :: ArmorConfig
defConfig = ArmorConfig "test-data" 1


------------------------------------------------------------------------------
-- | Tests the serialization backwards compatibility of a data type by storing
-- serialized representations in .test files to be checked into your project's
-- version control.
--
-- First, this function checks the directory 'ccStoreDir' for the existence of
-- a file @foo-000.test@.  If it doesn't exist, it creates it for each
-- serialization with the serialized representation of the val parameter.
--
-- Next, it checks that the serialized formats in the most recent
-- 'ccNumVersions' of the stored @.test@ files are parsable by the current
-- version of the serialization.
testArmor
    :: (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> String
    -> a
    -> Test
testArmor cc valId val =
    TestList [ testIt s | s <- M.toList serializations ]
  where
    testIt s = test (testSerialization cc valId val s)


------------------------------------------------------------------------------
testSerialization
    :: forall a. (Eq a, Show a, Typeable a, Armored a)
    => ArmorConfig
    -> String
    -> a
    -> (String, APrism' ByteString a)
    -> Assertion
testSerialization cc valId val s@(_,p) = do
    let d = getVersionDir cc val s
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
    vs = reverse [unVersion curVer - ccNumVersions cc .. unVersion curVer]
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
getVersionDir cc val (nm,_) = ccStoreDir cc </> show (typeOf val) </> nm
