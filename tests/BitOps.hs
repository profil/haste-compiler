{-# LANGUAGE CPP #-}
module Tests.BitOps where

import Data.Word
import Data.Bits

#include "testdata.incl"

test :: Word -> Word -> [String]
test a b = [ show (a .&. b),
             show (a .|. b),
             show (a `xor` b),
             show ((a .&. 0xffff) `shiftL` 2),
             show (a `shiftR` 3) ]
main :: IO ()
main = do
  print $ map (\(a,b) -> test a b) testData

