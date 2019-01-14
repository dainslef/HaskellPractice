{-# LANGUAGE BangPatterns #-}

import Data.Foldable (foldl')

owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl = ((.)$(.))

dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = ((.).(.))

-- squish ::
-- squish = f >>= a . b . c =<< g

main :: IO ()
main = print "Pointfree"
