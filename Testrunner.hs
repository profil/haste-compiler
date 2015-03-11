module Testrunner where

-- Run tests
import Control.Applicative
import Control.Monad
import System.Directory
import System.IO
import System.Process
import Test.QuickCheck

type Name     = String
type TestData = String

data Test = Test Name TestData
  deriving Show

-- | Runs a test.
runTest :: Test -> IO ()
runTest (Test name testData) = do
  tp <- (++ "/Tests/") <$> getCurrentDirectory

  -- There's nothing you can't solve with some sed.
  callCommand $ "sed -e 's/#include \"testdata.incl\"/testData = " ++ testData ++ "/' " ++ tp ++ name ++ ".hs > " ++ tp ++ name ++ ".t"

  -- 2. Run the Javascript version of the test.
  callProcess "hastec" [
    "-fforce-recomp",
    "--opt-whole-program",
    "-DO2",
    "--onexec",
    tp ++ name ++ ".t"]
  hastecResult <- readProcess "node" [tp ++ name ++ ".js"] ""
  putStrLn $ "Haste says:\t" ++ hastecResult

  -- 3. Run the Haskell version of the test.
  ghcResult <- readProcess "runghc" [
    "-no-user-package-db",
    "-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/",
    tp ++ name ++ ".t"] ""
  putStrLn $ "GHC says:\t" ++ ghcResult

  -- 4. Compare the results.
  if hastecResult == ghcResult
     then putStrLn "Results are equal!"
     else putStrLn "Results differ!"

  putStrLn ""

newTest :: (Show a) => Name -> Gen a -> IO Test 
newTest name gen = do
  t <- show <$> (generate gen)
  return $ Test name t
 
testList :: IO [Test]
testList = sequence [ newTest "Addition" (arbitrary :: Gen (Double, Double)) ]

main :: IO ()
main = do
  -- 1. Create testdata.
  testList >>= mapM_ runTest

