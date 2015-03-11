module Testrunner where

-- Run tests
import Control.Applicative
import Control.Monad
import System.Directory
import System.IO
import System.Process
import Test.QuickCheck

type Name = String

-- Take a name and a generator
data (Gen a) => Test a = Test Name a
  deriving Show

testPath :: IO FilePath
testPath = (++ "/tests/") <$> getCurrentDirectory

-- 2. Run the Haskell version of the test.
-- 3. Run the Javascript version of the test.
-- 4. Compare the results.
runTest :: Name -> IO ()
runTest p = do
  callProcess "hastec" ["-fforce-recomp", "--opt-whole-program", "-DO2", "--onexec", "--with-js=" ++ testPath ++ "TestData.js", testPath ++ p ++ ".hs", "-main-is", "Tests." ++  ++ ".main"]
  hastecResult <- readProcess "node" ["Test.js"] ""
  putStrLn "Haste says:\t" ++ hastecResult

  ghcResult <- readProcess "runghc" ["-no-user-package-db", "-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/", testPath ++ p ++ ".hs"] ""
  putStrLn "GHC says:\t" ++ ghcResult

  if hastecResult == ghcResult
     then putStrLn "Results are equal!"
     else putStrLn "Results differ!"

  putStrLn ""

testList = [ Test "Addition" (oneof $ [Double <$> arbitrary, Int <$> arbitrary])
           ,
           ]

createTestData :: Gen a -> IO ()
createTestData gen = do
  (++ "/tests/TestData.hs") <$> getCurrentDirectory
                            >>= writeFile ("module Tests.TestData where\n\n\ntestData = return " ++ (show $ generate gen))

main :: IO ()
main = do
  -- 1. Create testdata.
  mapM_ (\(Test name gen) -> createTestData gen >> runTest name) testList

