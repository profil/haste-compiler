{-# LANGUAGE CPP, OverloadedStrings #-}
module Tests.Test where

#ifdef __HASTE__
import Haste.Foreign
import Haste.Prim
testData :: IO [(Double, Double)]
testData = ffi "testData"
#else
import Tests.TestData (testData)
#endif

test :: (Double ,Double) -> Double
test (a, b) = a * b

main = do
  xs <- testData
  mapM_ (putStrLn . show . test) xs
