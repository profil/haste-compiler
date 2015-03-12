{-# LANGUAGE CPP #-}
module Tests.DoubleConversion where

{-# NOINLINE testData #-}
#include "testdata.incl"

main :: IO ()
main = print $ map floor testData ++ map ceiling testData ++ map round testData
