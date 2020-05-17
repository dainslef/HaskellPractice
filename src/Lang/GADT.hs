{-# LANGUAGE GADTs #-}

module Lang.GADT where

data Expr a =
  NumExpr a |
  BoolExpr a |
  Add (Expr a) (Expr a) |
  Eq (Expr a) (Expr a) deriving (Show, Eq)

-- with the GADT extension, the construction can define the return type with specifying type parameter
data GadtExpr a where
  NumGadtExpr :: Num a => a -> GadtExpr a
  BoolGadtExpr :: Bool -> GadtExpr Bool
  GadtAdd :: Num a => GadtExpr a -> GadtExpr a -> GadtExpr a
  GadtEq :: (Show a, Eq a) => GadtExpr a -> GadtExpr a -> GadtExpr Bool

instance Show a => Show (GadtExpr a) where
  show (NumGadtExpr  a) = "Num " ++ show a
  show (BoolGadtExpr a) = "Bool " ++ show a
  show (GadtAdd expr1 expr2) =
    show $ "GadtAdd " ++ show expr1 ++ " " ++ show expr2
  show (GadtEq expr1 expr2) =
    show $ "GadtEq " ++ show expr1 ++ " " ++ show expr2

getGadtExpr :: GadtExpr a -> a
getGadtExpr (NumGadtExpr  a                           ) = a
getGadtExpr (BoolGadtExpr a                           ) = a
getGadtExpr (GadtAdd (NumGadtExpr n1) (NumGadtExpr n2)) = n1 + n2
getGadtExpr (GadtEq expr1 expr2) = getGadtExpr expr1 == getGadtExpr expr2

p :: Show a => GadtExpr a -> IO ()
p s = print $ getGadtExpr s

testGADT :: IO ()
testGADT = do
  let (num1, num2)   = (NumGadtExpr 10, NumGadtExpr 20)
  let (bool1, bool2) = (BoolGadtExpr True, BoolGadtExpr False)
  print $ GadtEq bool1 bool2
  print $ GadtAdd num1 num2
  p $ GadtAdd (NumGadtExpr 10) (NumGadtExpr 20)
