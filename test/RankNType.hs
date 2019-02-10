{-# LANGUAGE RankNTypes, FlexibleInstances #-}

rankNTypes :: (Show a, Show b) =>
  (forall a . Show a => a -> String) -> a -> b -> IO ()
rankNTypes f a b = do
  let (fa, fb) = (f a, f b)
  print $ "a: " ++ fa ++ " b: " ++ fb

main :: IO ()
main = rankNTypes show 666 [1, 2, 3, 4]
