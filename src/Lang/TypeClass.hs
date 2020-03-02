{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-} -- 令 TypeClass 支持多个类型参数
{-# LANGUAGE FunctionalDependencies #-} -- 令 TypeClass 支持类型参数依赖

module Lang.TypeClass where

import Text.Read (read)

newtype TypeA = TypeA Int
newtype TypeB = TypeB String



class TypeDependency a b c where
  get :: a -> b -> c

instance TypeDependency TypeA TypeB String where
  get (TypeA a) (TypeB b) = "A:" ++ show a ++ ", B:" ++ b

instance TypeDependency TypeA TypeA String where
  get (TypeA a1) (TypeA a2) = "A1:" ++ show a1 ++ ", A2:" ++ show a2

testTypeClass1 = do
  print (get (TypeA 666) (TypeB "2333") :: String)
  print (get (TypeA 666) (TypeA 777) :: String)



class TypeClass a b where
  doSomething :: a -> b -> IO ()

instance TypeClass TypeA TypeB where
  doSomething (TypeA t) _ = print $ "Type Class AB: " ++ show t

-- 使用语言扩展 FlexibleInstances 开启泛型参数特化
instance TypeClass TypeA TypeA where
  doSomething (TypeA t) _ = print $ "Type Class AA: " ++ show t

testTypeClass2 = do
  doSomething (TypeA 2333) (TypeB "2333")
  doSomething (TypeA 666) (TypeA 23333)



class MultiParamTypeClasses a b | a -> b where
  m :: a -> a -> b

-- 使用 FunctionalDependencies 需要保证在依赖路径下只有一个 instance 实现，以次避免显式指定类型
-- instance MultiParamTypeClasses String String where m = (++)
instance MultiParamTypeClasses String Int where m = flip ((+) . read) . read

testTypeClass3 = do
  -- print (m "123" "456")
  print $ m "777" "666"



class FunctionalDependencies argA argB argC | argB -> argC where
  f :: argA -> argB -> argC

instance FunctionalDependencies String String String where f = (++)
instance FunctionalDependencies Int String String where f = (++) . show

testTypeClass4 = do
  print $ f "123" "456"
  print $ f (123 :: Int) "456"
