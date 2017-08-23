module Main where

------------------------------------------------------------------------------
import Armor
import Data.Typeable
import GHC.Generics
import System.Directory
import Test.HUnit
------------------------------------------------------------------------------
import qualified AppA as A
import qualified AppB as B
------------------------------------------------------------------------------

conf :: CompatConfig
conf = defConfig

aTests :: Test
aTests = TestList
    [ testBackwardsCompat conf "e1" (A.Employee "Bob" "Smith" (5,6))
    , testBackwardsCompat conf "e" A.Executive
    , testBackwardsCompat conf "m" A.Manager
    , testBackwardsCompat conf "w" A.Worker
    ]

bTests :: Test
bTests = TestList
    [ testBackwardsCompat conf "e1" (B.Employee "Bob" "Smith" 5 Nothing)
    , testBackwardsCompat conf "e" B.Executive
    , testBackwardsCompat conf "m" B.Manager
    , testBackwardsCompat conf "w" B.Worker
    ]

main :: IO ()
main = do
    _ <- runTestTT aTests
    return ()
    --removeDirectoryRecursive (ccStoreDir conf)
