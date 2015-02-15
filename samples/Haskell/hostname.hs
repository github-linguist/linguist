import Network.BSD
main = do hostName <- getHostName
          putStrLn hostName
