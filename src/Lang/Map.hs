module Lang.Map where

-- use function fold* to impelement function map
customMapL, customMapR :: (a -> b) -> [a] -> [b]
customMapR f = foldr (\a b -> f a : b) []
customMapL f = foldl (flip $ (:) . f) [] . reverse -- pointfree style
-- customMapL f l = foldl (\b a -> f a : b) [] $ reverse l -- normal style

testCustomMap = print a >> print b
  where
    a = customMapL (+1) [1, 2, 3]
    b = customMapR ((-)1) [1, 2, 3]
