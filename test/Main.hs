module Main where

------------------------------------------------------------------------------
import           Armor
import           System.Directory
import           Test.HUnit
------------------------------------------------------------------------------
import TestAppA
import TestAppB
------------------------------------------------------------------------------

conf :: ArmorConfig
conf = defConfig

main :: IO ()
main = do
    removeDirectoryRecursive (acStoreDir conf)
    acount <- runTestTT (aTests conf)
    bcount <- runTestTT (bTests conf)
    putStrLn $ "A: " ++ showCounts acount
    putStrLn $ "B: " ++ showCounts bcount
    return ()

