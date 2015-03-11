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

-- Take a name and a generator
data Test = Test Name TestData
  deriving Show

testPath :: IO FilePath
testPath = (++ "/Tests/") <$> getCurrentDirectory

-- 2. Run the Haskell version of the test.
-- 3. Run the Javascript version of the test.
-- 4. Compare the results.
runTest :: Name -> IO ()
runTest p = do
  tp <- testPath
  callProcess "hastec" [
    "-fforce-recomp",
    "--opt-whole-program",
    "-DO2",
    "--onexec",
    tp ++ p ++ ".hs"]
  hastecResult <- readProcess "node" [tp ++ p ++ ".js"] ""
  putStrLn $ "Haste says:\t" ++ hastecResult

  ghcResult <- readProcess "runghc" [
    "-no-user-package-db",
    "-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/",
    tp ++ p ++ ".hs"] ""
  putStrLn $ "GHC says:\t" ++ ghcResult

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

exportTestData :: TestData -> IO ()
exportTestData s = do
  let s' = "testData = " ++ s ++ "\n"
  putStr s'
  (++ "testdata.incl") <$> testPath
                     >>= \fn -> writeFile fn s'

main :: IO ()
main = do
  -- 1. Create testdata.
  testList >>= mapM_ (\(Test name td) -> exportTestData td >> runTest name) 

