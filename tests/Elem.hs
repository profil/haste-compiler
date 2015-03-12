{-# LANGUAGE CPP #-}
module Tests.Elem where

#include "testdata.incl"

test :: String -> String -> (Bool, Bool)
test a b = (elem '.' a, not $ elem '.' b)


main :: IO ()
main = do
  print $ map (\(a,b) -> test a b) testData

