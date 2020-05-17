{-# LANGUAGE FlexibleInstances #-} -- make type class allow the nested types that don't in turn contain type variables
-- {-# LANGUAGE MultiParamTypeClasses #-} -- make type class allow multi type parameters, it has aready imported by extension `FunctionalDependencies`
{-# LANGUAGE FunctionalDependencies #-} -- make type class support functional dependencies

module Lang.TypeClass where

import           Text.Read                      ( read )

newtype TypeA = TypeA Int
newtype TypeB = TypeB String



class TypeDependency a b c where
  get :: a -> b -> c

instance TypeDependency TypeA TypeB String where
  get (TypeA a) (TypeB b) = "A:" ++ show a ++ ", B:" ++ b

instance TypeDependency TypeA TypeA String where
  get (TypeA a1) (TypeA a2) = "A1:" ++ show a1 ++ ", A2:" ++ show a2

testTypeClass1 = do
  -- without functional dependencies you have specify the return type of the expression
  print (get (TypeA 666) (TypeB "2333") :: String)
  print (get (TypeA 666) (TypeA 777) :: String)



class TypeClass a b where
  doSomething :: a -> b -> IO ()

instance TypeClass TypeA TypeB where
  doSomething (TypeA t) _ = print $ "Type Class AB: " ++ show t

-- use extension `FlexibleInstances` to allow nested types that don't in turn contain type variables
instance TypeClass TypeA TypeA where
  doSomething (TypeA t) _ = print $ "Type Class AA: " ++ show t

testTypeClass2 = do
  doSomething (TypeA 2333) (TypeB "2333")
  doSomething (TypeA 666)  (TypeA 23333)



class MultiParamTypeClasses a b | a -> b where
  m :: a -> a -> b

-- use extension `FunctionalDependencies`, the dependency path should be unique,
-- one dependency can only be allowed to have on `instance`
instance MultiParamTypeClasses String Int where
  m = flip ((+) . read) . read
-- error: ""
-- instance MultiParamTypeClasses String String where m = (++)

testTypeClass3 = print $ m "123" "456"



class FunctionalDependencies argA argB argC | argB -> argC where
  f :: argA -> argB -> argC

instance FunctionalDependencies String String String where
  f = (++)
instance FunctionalDependencies Int String String where
  f = (++) . show

testTypeClass4 = do
  print $ f "123" "456"
  print $ f (123 :: Int) "456"
