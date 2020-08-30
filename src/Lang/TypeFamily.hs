{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
-- MultiParamTypeClasses implied by this
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Lang.TypeFamily where

import Text.Read (read)

type family ClosedFamily a where
  ClosedFamily String = String
  ClosedFamily Int = Int
  ClosedFamily (Maybe a) = Maybe a
  ClosedFamily [a] = [a]

testClosed :: Show a => ClosedFamily [a] -> IO ()
testClosed = print . show

type family OpenFamily a

-- type instance OpenFamily a = [a]
type instance OpenFamily String = String

type instance OpenFamily Int = Int

type instance OpenFamily (Maybe a) = Maybe a

testString :: OpenFamily String -> IO ()
testString = print

testInt :: OpenFamily Int -> IO ()
testInt = print . show

testTypeFamily1 = do
  testString "2333"
  testInt 2333

data family DataFamily a

-- use `newtype` keyword to define a new type when the type has only one parameter
newtype instance DataFamily Int = FInt Int

-- the argument in constructor can use the other type from current type family
data instance DataFamily String = FString (DataFamily Int) String

-- each `instance` can `deriving` the different type classes
data instance DataFamily (Maybe a) = FJust a | FNothing deriving (Show, Eq)

testFamilyInt :: DataFamily Int -> IO ()
testFamilyInt (FInt a) = print . show $ a

testFamilyMaybe :: Show a => DataFamily (Maybe a) -> IO ()
testFamilyMaybe f@(FJust a) = print $ "FJust: " ++ show f
testFamilyMaybe FNothing = print "Nothing"

data family Expr a

data instance Expr a where
  Num :: Num a => a -> Expr a
  Bool :: Bool -> Expr Bool
  Add :: Num a => Expr a -> Expr a -> Expr a
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

class TypeFamily a where
  type Family a
  f :: Family a -> a

instance TypeFamily String where
  type Family String = [String]
  f = head

testTypeFamily2 = do
  testFamilyInt $ FInt 2333
  testFamilyMaybe $ FJust 6666
  testFamilyMaybe (FNothing :: DataFamily (Maybe String))

-- The feature "functional dependencies" can help compiler to infer the type info precisely,
-- remove the functional dependencies or change the dependencies to "b -> a",
-- will cause the type inference failed
class TypeClass a b | a -> b where
  t :: a -> b

instance TypeClass String Int where
  t = read

testTypeFamily3 = do
  print $ show (f ["abc"] :: String)
  print . show . t $ "2333"

data family KindA a

-- Kind, need two type parameter
data family Kind1 :: * -> * -> *

data family DataKindA1 a

data instance Kind1 Int String = DataKind1

data instance DataKindA1 String = DataKindA1 String deriving (Show)

testTypeFamily4 = print $ DataKindA1 "2333"
