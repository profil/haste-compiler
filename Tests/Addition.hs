module Tests.Addition where
-- * Test addition of two numbers.

#include "testdata.incl"

addition :: (Num a) => a -> a -> a
addition a b = a + b

main :: IO ()
main = do
  putStrLn "Haste bryr sig inte om vad jag skriver. Alltid samma output :("
  print $ addition (fst testData) (snd testData)

