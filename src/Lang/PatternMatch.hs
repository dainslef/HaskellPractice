module Lang.PatternMatch where

newtype Number = Number {
  value :: Int
} deriving (Eq, Show)

printNumber :: Number -> IO ()
printNumber (Number 2333) = print "2333" -- 使用构造器解构
printNumber num@(Number n) -- 解构并绑定名称
  | elem n [1, 2, 3] = print "n in 1, 2, 3"
  | n > 10 = print "n > 10"
printNumber _ = print "No match"

add :: Int -> Int -> Int
add n | n > 100 = (+n)
-- add n1 n2 = n1 + n2 -- 模式匹配中的每一个表达式需要有相同的参数数目

testPatternMatch = do
  printNumber $ Number 2333
  printNumber $ Number 1
