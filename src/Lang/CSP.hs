module Lang.CSP where

fab :: Int -> Int
fab n | n == 0 || n == 1 = n
fab n                    = fab (n - 1) + fab (n - 2)

fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)

printAll :: IO ()
printAll = foldMap print [fab 10, fac 10, facCSP 10 id]

-- fabCSP :: Int -> (Int -> r) -> r
-- fabCSP i k | i == 0 || i == 1 = k i
-- fabCSP i k = fabCSP (i - 1) $ \r -> k ()

facCSP :: Int -> (Int -> r) -> r
facCSP 1 k = k 1
facCSP i k = facCSP (i - 1) $ \r -> k (r * i)

{-- computation steps
facCSP 10 k = facCSP 9 $ \r -> k (r * 10) -- facCSP 9 $ \r -> id (r * 10)
facCSP 9 k = facCSP 8 $ \r -> k (r * 9) -- facCSP 8 $ \r -> id ((r * 9) * 10)
...
facCSP 2 k = facCSP 2 $ \r -> k (r * 1) -- facCSP 2 $ \r -> id (((r * 2) ... * 9) * 10)
facCSP 1 k = k 1 -- id (((1 * 2) ... * 9) * 10)
--}

sum :: [Int] -> Int
sum = k 0 where
  k v []      = v
  k v (n : l) = k (v + n) l

sum' :: [Int] -> Int
sum' [v    ] = v
sum' (v : l) = v + sum' l

sumCSP :: [Int] -> (Int -> r) -> r
sumCSP [n    ] f = f n
sumCSP (n : l) f = sumCSP l $ f . (n +)

f :: (Int -> Int) -> Int -> Int
f f' = f'

testCSP :: IO ()
testCSP = sumCSP [1 .. 100] print
