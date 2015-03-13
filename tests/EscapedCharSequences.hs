{-# LANGUAGE CPP #-}
module Tests.EscapedCharSequences where
import Data.Char (ord)

{-# NOINLINE testData #-}
#include "testdata.incl"

{-# NOINLINE testData2 #-}
testData2 = testData ++ testData

-- Standalone SpiderMonkey can't handle showing Unicode chars, so we have to
-- look at the char codes. :(
main :: IO ()
main = print testData2

