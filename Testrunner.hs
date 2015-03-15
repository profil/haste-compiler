module Testrunner where

-- Run tests
import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import System.Directory
import System.IO
import System.Posix.Files
import System.Process
import Test.QuickCheck

import Data.Word
import Data.Int

type Name     = String
type TestData = String

data Test = Test Name TestData
  deriving Show

-- | Runs a test.
runTest :: Test -> IO String
runTest (Test name testData) = do
  currDir <- getCurrentDirectory
  let tp = currDir ++ "/tests/"
  let ip = tp ++ "includedir" ++ name ++ "/"

  -- Create a directory for the included testdata file.
  -- With this we can handle tests running in parallell.
  createDirectoryIfMissing False ip

  -- 1. Create testdata.
  let testFile = ip ++ "testdata.incl"
  writeFile testFile $ "testData = " ++ testData

  -- 2. Run the Javascript version of the test.
  -- Quickfix a bug by changing directory to the same as the tests are in.
  let args = proc "hastec" [ "-fforce-recomp"
                           , "--opt-whole-program"
                           , "-DO2"
                           , "--onexec"
                           , "-I" ++ ip
                           , "-main-is"
                           , "Tests." ++ name
                           , tp ++ name ++ ".hs"]
  (_,_,_,p) <- createProcess $ args { cwd = Just tp }
  waitForProcess p

  hastecResult <- readProcess "node" [tp ++ name ++ ".js"] ""

  -- 3. Run the Haskell version of the test.
  ghcResult <- readProcess "runghc" [
    "-no-user-package-db",
    "-I" ++ ip,
    "-package-db=.cabal-sandbox/x86_64-linux-ghc-7.8.4-packages.conf.d/",
    tp ++ name ++ ".hs"] ""

  removeDirectoryRecursive ip
--  removeFile $ tp ++ name ++ ".js"

  let result = [ "================================================================================"
               , "Running test: " ++ name
               , "================================================================================"
               , ""
               , "Haste says:\t" ++ hastecResult
               , "GHC says:\t" ++ ghcResult
               , ""
               ]

  -- 4. Compare the results and return.
  if hastecResult == ghcResult
     then return $ join $ result ++ ["Results are equal!", ""]
     else return $ join $ result ++ ["Results differ!", "Testdata: " ++ testData]


newTest :: (Show a) => Name -> IO [a] -> IO Test
newTest name td = (\t -> Test name $ show t) <$> td

escapeCharacters :: [Char]
escapeCharacters = concat [ "\NUL", "\DLE", "\SOH", "\DC1", "\STX", "\DC2"
                   , "\ETX", "\DC3", "\EOT", "\DC4", "\ENQ", "\NAK"
                   , "\ACK", "\SYN", "\BEL", "\ETB", "\BS",  "\CAN"
                   , "\EM",  "\LF",  "\SUB", "\VT",  "\ESC"
                   , "\FF",  "\FS",  "\CR",  "\GS",  "\SO",  "\RS"
                   , "\SI",  "\US",  "\DEL", "\0",   "\a",   "\b"
                   , "\f",   "\n",   "\r",   "\t",   "\v",   "\""
                   , "\"",   "\\", "\x1234", "\&56"
                   ]

alfanumericCharacters :: [Char]
alfanumericCharacters = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ [' ']

testList :: IO [Test]
testList = sequence
  [ newTest "Addition" (sample' (arbitrary :: Gen (Double, Double)))
  , newTest "Addition" (sample' (arbitrary :: Gen (Int, Double)))
  , newTest "Addition" (sample' (arbitrary :: Gen (Int, Int)))
  , newTest "BitOps"   (sample' (arbitrary :: Gen (Word, Word)))
  , newTest "ComplexFromIntegral" (sample' (arbitrary :: Gen (Double, Double, Double, Double)))
  , newTest "ComplexFromIntegral" (sample' (arbitrary :: Gen (Int, Int, Int, Int)))
  , newTest "DoubleConversion" (sample' (arbitrary :: Gen (Int)))
  , newTest "DoubleDiv" (sample' (arbitrary :: Gen (Double, Double, Double)))
  , newTest "DoubleDiv" (sample' (arbitrary :: Gen (Int, Int, Int)))
  , newTest "Elem"     (sample' (arbitrary :: Gen (String, String)))
  , newTest "EscapedCharSequences" (sample' $ elements (escapeCharacters ++ alfanumericCharacters))
  , newTest "Int64"    (sample' (arbitrary :: Gen (Int64, Int64)))
  ]

main :: IO ()
main = do
  tests <- testList
  mapConcurrently runTest tests >>= (putStrLn . concat)

