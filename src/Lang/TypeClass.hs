-- make type class allow the nested types that don't in turn contain type variables
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-} -- make type class allow multi type parameters, it has aready imported by extension `FunctionalDependencies`
-- make type class support functional dependencies
{-# LANGUAGE FunctionalDependencies #-}

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
  -- without functional dependencies you have specify the return type of the expression
  print (get (TypeA 666) (TypeB "2333") :: String)
  print (get (TypeA 666) (TypeA 777) :: String)

class TypeClass a b where
  overload :: a -> b -> IO ()

instance TypeClass TypeA TypeB where
  overload (TypeA t) _ = print $ "Type Class AB: " ++ show t

-- use extension `FlexibleInstances` to allow nested types that don't in turn contain type variables
-- overload method with different type of parameter in argument list (ad hoc polymorphism)
instance TypeClass TypeA TypeA where
  overload (TypeA t) _ = print $ "Type Class AA: " ++ show t

testTypeClass2 = do
  overload (TypeA 2333) (TypeB "2333")
  overload (TypeA 666) (TypeA 23333)

class TypeClassWithDynamicArgs a where
  dynamicArgs :: a

instance TypeClassWithDynamicArgs (TypeA -> IO ()) where
  dynamicArgs = \(TypeA a) -> print $ "Type Class A: " ++ show a

instance TypeClassWithDynamicArgs (TypeB -> IO ()) where
  dynamicArgs = \(TypeB b) -> print $ "Type Class B: " ++ b

instance TypeClassWithDynamicArgs (TypeA -> TypeB -> IO ()) where
  dynamicArgs = \(TypeA a) (TypeB b) -> print $ "Type Class A: " ++ show a ++ ", Class B: " ++ b

-- overload method with different number of parameter in argument list (parametric polymorphism)
testTypeClassWithDynamicArgs = do
  dynamicArgs (TypeA 666) :: IO ()
  dynamicArgs (TypeB "2333") :: IO ()
  dynamicArgs (TypeA 666) (TypeB "2333") :: IO ()

class MultiParamTypeClasses a b | a -> b where
  m :: a -> a -> b

-- use extension `FunctionalDependencies`, the dependency path should be unique,
-- one dependency can only be allowed to have on `instance`
instance MultiParamTypeClasses String Int where
  m = flip ((+) . read) . read

-- error: ""
-- instance MultiParamTypeClasses String String where m = (++)

testFunctionalDependencies1 = print $ m "123" "456"

class FunctionalDependencies argA argB argC | argB -> argC where
  f :: argA -> argB -> argC

instance FunctionalDependencies String String String where
  f = (++)

instance FunctionalDependencies Int String String where
  f = (++) . show

testFunctionalDependencies2 = do
  print $ f "123" "456"
  print $ f (123 :: Int) "456"
