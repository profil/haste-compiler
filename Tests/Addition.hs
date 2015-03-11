-- * Test addition of two numbers.

addition :: (Num a) => a -> a -> a
addition a b = a + b

main :: IO ()
main = do
  addition a b

