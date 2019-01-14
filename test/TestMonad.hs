{-# LANGUAGE ApplicativeDo #-}

data Mon a = Mon {
  mon :: a
} deriving (Eq, Show)

instance Functor Mon where
  fmap f fa = Mon $ f $ mon fa

instance Applicative Mon where
  pure = Mon
  fab <*> fa = Mon $ mon fab $ mon fa

instance Monad Mon where
  ma >>= amb = amb $ mon ma

data App a = App {
  app :: a
} deriving (Eq, Show)

instance Functor App where
  fmap  = (App.) . (.app)

instance Applicative App where
  pure = App
  (<*>) = flip (flip (App.) . app) . app

main :: IO ()
main = print app1 >> print app2 >> print mon1 where
  app1 = (App . (++) . (.show)) <$> App "abc" <*> App 1
  app2 = do -- ApplicativeDo 特性
    a <- App "abc"
    b <- App 2
    pure $ a ++ (show b)
  mon1 = do
    a <- Mon "abc"
    b <- Mon 3
    Mon $ a ++ (show b)
