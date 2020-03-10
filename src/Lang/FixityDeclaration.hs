module Lang.FixityDeclaration where

testFixityDeclaration :: IO ()
testFixityDeclaration = do
  print "Custom operate (...):"
  return 1 ... return 2 ... return 3 -- 操作符左結合
  print "Custom operate (|||):"
  return 1 ||| return 2 ||| return 3 -- 操作符右結合
  print "Custom operate (@@@):"
  (return 1 @@@ return 2) @@@ return 3 -- 操作符不可結合
  return ()

(...), (|||) :: IO Int -> IO Int -> IO Int
n1 ... n2 = do
  v1 <- n1
  v2 <- n2
  print $ "left: " ++ show v1 ++ " right: " ++ show v2
  return $ v1 + v2
(|||) = (...)
(@@@) = (...)

-- 定義操作符結合性和優先級
infixl 5 ...
infixr 6 |||
infix 7 @@@
