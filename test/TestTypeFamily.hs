{-# LANGUAGE TypeFamilies #-}

data family KindA a
data family Kind1 :: * -> * -> *
data family DataKindA1 a

data instance Kind1 Int = DataKind1
data instance DataKindA1 String = DataKindA1 String deriving Show

main :: IO ()
main = print $ DataKindA1 "2333"
