module Main where

------------------------------------------------------------------------------
import           Armor
import           Control.Monad
import           System.Directory
import           Test.HUnit
import           Test.Hspec
------------------------------------------------------------------------------
import           TestAppA
import           TestAppB
------------------------------------------------------------------------------

conf :: ArmorConfig
conf = defArmorConfig

main :: IO ()
main = do
    exists <- doesDirectoryExist (acStoreDir conf)
    when exists $ removeDirectoryRecursive (acStoreDir conf)
    acount <- runTestTT (aTests conf)
    bcount <- runTestTT (bTests conf)
    putStrLn $ "A: " ++ showCounts acount
    putStrLn $ "B: " ++ showCounts bcount
    hspec $ describe "armor tests" $ do
      it "correctly handles AppA" $
        Counts 8 8 0 0 == acount
      it "correctly handles AppB" $
        Counts 10 10 0 1 == bcount
    removeDirectoryRecursive (acStoreDir conf)

