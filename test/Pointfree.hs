{-# LANGUAGE BangPatterns #-}

import Data.Foldable (foldl')

-- 进行pointfree变换时的简单规律：
-- 1.表达式是从左向右结合的，函数签名最后的类型变形需要在表达式的最左端进行
-- 2.使用.函数组合函数或变更函数签名
-- 3.灵活使用flip函数调换参数位置，使表达式的参数位置匹配签名位置
-- 4.当表达式结果类型完全匹配函数签名时则变形完成

mySum, mySumP :: [Int] -> Int
mySum l = foldl (+) 0 l
mySumP = foldl (+) 0 -- Pointfree Style

owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl = ((.) $ (.))

dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = ((.) . (.))

squish :: (b1 -> a1)
  -> (a1 -> b1 -> c)
  -> (b2 -> b1)
  -> (a2 -> a3 -> b2)
  -> (a3 -> a2)
  -> a3
  -> c
squish f a b c g = (f >>= a) . b . (c =<< g)

main :: IO ()
main = print "Pointfree"
