module Lang.PatternMatch where

newtype Number = Number
  { value :: Int
  }
  deriving (Eq, Show)

printNumber :: Number -> IO ()
-- use constructor to deconstruct
printNumber (Number 2333) = print "2333"
-- destruction and name binding
printNumber num@(Number n)
  | n `elem` [1, 2, 3] = print "n in 1, 2, 3"
  | n > 10 = print "n > 10"
printNumber _ = print "No match"

add :: Int -> Int -> Int
add n | n > 100 = (+ n)

-- add n1 n2 = n1 + n2 -- every expression in partten match should have same size of arguments

testPatternMatch = do
  printNumber $ Number 2333
  printNumber $ Number 1
