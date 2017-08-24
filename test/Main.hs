module Main where

------------------------------------------------------------------------------
import           Armor
import           Data.Typeable
import           GHC.Generics
import           System.Directory
import           Test.HUnit
------------------------------------------------------------------------------
import qualified AppA as A
import qualified AppB as B
import TestAppA
import TestAppB
------------------------------------------------------------------------------

conf :: ArmorConfig
conf = defConfig

main :: IO ()
main = do
    _ <- runTestTT (aTests conf)
    return ()
