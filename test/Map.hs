-- 使用 fold 操作实现 map
customMapL, customMapR :: (a -> b) -> [a] -> [b]
customMapR f = foldr (\a b -> f a : b) []
customMapL f = foldl (flip $ (:) . f) [] . reverse -- Pointfree 风格
-- customMapL f l = foldl (\b a -> f a : b) [] $ reverse l -- 普通风格

main :: IO ()
main = print a >> print b
  where
    a = customMapL (+1) [1, 2, 3]
    b = customMapR ((-)1) [1, 2, 3]
