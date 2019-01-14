{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}

class TypeClass t where
  doSomething :: t -> IO ()

instance TypeClass Int where
  doSomething t = print $ "Int Type Class " ++ (show t)

-- 使用语言扩展 FlexibleInstances 开启泛型参数特化
instance TypeClass String where
  doSomething t = print $ "String Type Class " ++ t

class TypeDependencies a b | a -> b where
  t :: a -> b

instance TypeDependencies (String, String) String where
  t (s1, s2) = s1 ++ " " ++ s2

instance TypeDependencies Int String where t = show
instance TypeDependencies Int Int where t = id

class TypeFamily a where
  type Family a
  f :: a -> Family a

instance TypeFamily (String, String) where
  type Family (String, String) = String
  f (s1, s2) = s1 ++ " " ++ s2

class MultiParamTypeClasses a b c | b -> c where
  m :: a -> b -> c

instance MultiParamTypeClasses Int String String where
  m = (++) . show

instance MultiParamTypeClasses Int Int String where
  m = (flip $ (++) . show) . show

test1, test2 :: IO ()
test1 = do
  doSomething (666 :: Int)
  doSomething "2333"
test2 = do
  print $ t ("2333", "3333")
  print $ f ("2333", "6666")
test3 = do
  -- print (m (2333 :: Int) "2333" :: String)
  print $ m (2333 :: Int) "666"

main = test3
