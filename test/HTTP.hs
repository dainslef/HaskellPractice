import Network.HTTP.Client

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let request = parseRequest_ "GET http://www.baidu.com"
  httpLbs request manager >>= print
