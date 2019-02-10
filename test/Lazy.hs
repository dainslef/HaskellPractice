{-# LANGUAGE BangPatterns #-}

test1 :: IO ()
test1 = do
  let !f = (1+) $ error "Error Arg!"
  print "Before..."
  let s = f - 1
  print "After..."

test2 :: IO ()
test2 =
  let f = (1+) $! error "Error Arg!" in print "Before..." >>
    let s = f - 1 in print "After..."

doSomething :: Int -> IO ()
doSomething !n = do
  print "Before..."
  print $ "After: " ++ (show n) ++ "..."

data Test = Test !Int

test3 = do
  case (1, undefined) of
    !(a, b) -> print "Strict Out"
  case (1, undefined) of
    (a, !b) -> print "Strict Inner"

test4 = do
  let (a, b) = (1, undefined)
  print "Lazy"
  let !(a, b) = (1, undefined)
  print "Strict Out"
  let (a, !b) = (1, undefined)
  print "Strict Inner"
  let !(a, !b) = (1, undefined)
  print "Strict Out and Inner"

test5 = let !(a, !b) = (1, undefined) in print "Lazy"

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z [] = z
myFoldl f z (n:l) = myFoldl f (f z n) l

myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' f z [] = z
myFoldl' f z (n:l) = myFoldl' f (flip f n $! z) l

myFoldl'' :: (b -> a -> b) -> b -> [a] -> b
myFoldl'' f z [] = z
myFoldl'' f !z (n:l) = myFoldl'' f (f z n) l

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f !z (n:l) = f n $ myFoldr f z l

testFold = print $ myFoldr (-) 0 [1 .. 50000000]

main = testFold
