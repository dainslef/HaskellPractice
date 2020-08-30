module Lang.FixityDeclaration where

testFixityDeclaration :: IO ()
testFixityDeclaration = do
  print "Custom operate (...):"
  return 1 ... return 2 ... return 3 -- left-associative
  print "Custom operate (|||):"
  return 1 ||| return 2 ||| return 3 -- right-associative
  print "Custom operate (@@@):"
  (return 1 @@@ return 2) @@@ return 3 -- non-associative
  return ()

(...), (|||), (@@@) :: IO Int -> IO Int -> IO Int
n1 ... n2 = do
  v1 <- n1
  v2 <- n2
  print $ "left: " ++ show v1 ++ " right: " ++ show v2
  return $ v1 + v2
(|||) = (...)
(@@@) = (...)

-- fixity declarations
infixl 5 ...

infixr 6 |||

infix 7 @@@
