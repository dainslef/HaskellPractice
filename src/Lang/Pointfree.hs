module Lang.Pointfree where

import           Data.Foldable                  ( foldl' )

-- 進行pointfree變換時的簡單規律：
-- 1. 表達式從左向右結合，函數簽名最後的類型變形需要在表達式最左端進行
-- 2. 使用`.`函數組合函數或變更函數簽名
-- 3. 靈活使用`flip`函數調換參數位置，使表達式的參數位置匹配簽名位置
-- 4. 當表達式結果類型完全匹配函數簽名時則變形完成

mySum, mySumP :: [Int] -> Int
mySum l = foldl (+) 0 l -- normal style
mySumP = foldl (+) 0 -- pointfree style

owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl = (.) (.)

dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.) . (.)

squish
  :: (b1 -> a1)
  -> (a1 -> b1 -> c)
  -> (b2 -> b1)
  -> (a2 -> a3 -> b2)
  -> (a3 -> a2)
  -> a3
  -> c
squish f a b c g = (f >>= a) . b . (c =<< g)
