module Lang.HTTP where

import Network.HTTP.Client

testHttp :: IO ()
testHttp = do
  manager <- newManager defaultManagerSettings
  let request = parseRequest_ "GET http://www.baidu.com"
  httpLbs request manager >>= print
