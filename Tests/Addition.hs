{-# LANGUAGE CPP #-}
module Tests.Addition where
-- * Test addition of two numbers.

#include "testdata.incl"

addition :: (Num a) => a -> a -> a
addition a b = a + b

main :: IO ()
main = do
  let (a, b) = testData
   in print $ addition a b

