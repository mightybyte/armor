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
import           Data.Word
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
class Armored a where
  version :: Version a
  serializations :: Map String (APrism' ByteString a)


------------------------------------------------------------------------------
data CompatConfig = CompatConfig
    { ccStoreDir    :: FilePath
    , ccNumVersions :: Word
    }


defConfig :: CompatConfig
defConfig = CompatConfig "test-data" 1


------------------------------------------------------------------------------
testBackwardsCompat
    :: (Eq a, Show a, Typeable a, Armored a)
    => CompatConfig
    -> String
    -> a
    -> Test
testBackwardsCompat cc valId val =
    TestList [ testIt s | s <- M.toList serializations ]
  where
    testIt s = test (testSerialization cc valId val s)


------------------------------------------------------------------------------
testSerialization
    :: forall a. (Eq a, Show a, Typeable a, Armored a)
    => CompatConfig
    -> String
    -> a
    -> (String, APrism' ByteString a)
    -> Assertion
testSerialization cc valId val s@(nm,p) = do
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
getVersionFilename valId ver =
    printf "%s-%03d.test" valId (unVersion ver)


------------------------------------------------------------------------------
getVersionDir cc val (nm,p) = ccStoreDir cc </> show (typeOf val) </> nm
