{-# LANGUAGE CPP #-}
module Tests.ComplexFromIntegral where
import Data.Complex

#include "testdata.incl"

main :: IO ()
main = do
  print $ map (\(a,b,c,d) -> (a :+ b) * (c :+ d)) testData

