{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, AllowAmbiguousTypes, StandaloneDeriving #-}

-- 使用普通ADT无法对类型参数进行限定
-- data Expr a =
--   Num a | Bool a |
--   Add (Expr a) (Expr a) |
--   Eq (Expr a) (Expr a) deriving (Show, Eq)

data Expr a where
  Num :: Num a => a -> Expr a
  Bool :: Bool -> Expr Bool
  Add :: Num a => Expr a -> Expr a -> Expr a
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

getExpr :: Expr a -> a
getExpr (Num value) = value
getExpr (Bool value) = value
getExpr (Add (Num n1) (Num n2)) = n1 + n2
getExpr (Eq expr1 expr2) = (getExpr expr1) == (getExpr expr2)

p :: (Show a, Show b) =>
  (forall a . Show a => Expr a -> IO ()) -> (Expr a, Expr b) -> IO ()
p f (s1, s2) = f s1 >> f s2

data family FamilyExpr a
data instance FamilyExpr Int = IntExpr Int Int deriving Show
data instance FamilyExpr String = Empty | Str String | MultiStr [String] deriving Show

main :: IO ()
main = do
  let (expr1, expr2) = (Num (20 :: Int), Num (20 :: Int))
  p (print . getExpr) (Add expr1 expr2, Eq expr1 expr2)
  print $ show Empty
