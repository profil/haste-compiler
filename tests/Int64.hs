{-# LANGUAGE CPP #-}
module Tests.Int64 where

import Data.Int
import Data.Bits

#include "testdata.incl"

test :: Int64 -> Int64 -> [Int64]
test a b = [ a,
             a + b,
             a * b,
             if b /= 0 then a `div` b else a `div` (b+1),
             (a `xor` b) .&. 43042900,
             a * b + (a + b) ]


main :: IO ()
main = do
  print $ map (\(a,b) -> test a b) testData

