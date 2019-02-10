{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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

test1 = do
  testString "2333"
  testInt 2333



data family DataFamily a
newtype instance DataFamily Int = FInt Int  -- 构造器单参数时可使用 newtype 关键字
data instance DataFamily String = FString (DataFamily Int) String -- 构造器中的参数可依赖具体的类型族中的其它实例
data instance DataFamily (Maybe a) = FJust a | FNothing deriving (Show, Eq) -- 每格 instance 可以 deriving 各自的 TypeClass

testFamilyInt :: DataFamily Int -> IO ()
testFamilyInt (FInt a) = print . show $ a

testFamilyMaybe :: Show a => DataFamily (Maybe a) -> IO ()
testFamilyMaybe f@(FJust a) = print $ "FJust: " ++ (show f)
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

test2 = do
  testFamilyInt $ FInt 2333
  testFamilyMaybe $ FJust 6666
  testFamilyMaybe (FNothing :: DataFamily (Maybe String))

-- 去掉依赖关系或修改依赖关系为 b -> a 均会导致方法调用时类型自动推导失败
class TypeClass a b | a -> b where
  t :: a -> b

instance TypeClass String Int where
  t = read

test3 = do
  print $ show $ (f ["abc"] :: String)
  print . show . t $ "2333"



data family KindA a
data family Kind1 :: * -> * -> *
data family DataKindA1 a

data instance Kind1 Int = DataKind1
data instance DataKindA1 String = DataKindA1 String deriving Show

test4 = print $ DataKindA1 "2333"

main = test3
