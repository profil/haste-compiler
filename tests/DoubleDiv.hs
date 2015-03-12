{-# LANGUAGE CPP #-}
module Tests.DoubleDiv where
str = show . (round :: Double -> Int)

{-# NOINLINE testData #-}
testData :: [(Double, Double, Double)]
#include "testdata.incl"

main :: IO ()
main = putStrLn $ show $ map (\(a,b,c) -> [
  str (a/a), str (a/b), str (a/c),
  str (b/a), str (b/b), str (b/c),
  str (c/a), str (c/b), str (c/c)]) testData

