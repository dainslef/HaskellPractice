{-# LANGUAGE ApplicativeDo #-}

module Lang.Monad where

newtype Mon a = Mon {
  mon :: a
} deriving (Eq, Show)

instance Functor Mon where
  fmap f fa = Mon $ f $ mon fa

instance Applicative Mon where
  pure = Mon
  fab <*> fa = Mon $ mon fab $ mon fa

instance Monad Mon where
  ma >>= amb = amb $ mon ma

newtype App a = App {
  app :: a
} deriving (Eq, Show)

instance Functor App where
  fmap  = (App.) . (.app)

instance Applicative App where
  pure = App
  (<*>) = flip (flip (App.) . app) . app

testMonad :: IO ()
testMonad = print app1 >> print app2 >> print mon1 where

  -- normal use for Applicative
  app1 = App (flip (++) . show) <*> App 1 <*> App "abc"

  -- ApplicativeDo feautre
  app2 = do
    a <- App "abc"
    b <- App 2
    pure $ a ++ show b

  -- MonadDo
  mon1 = do
    a <- Mon "abc"
    b <- Mon 3
    Mon $ a ++ show b
