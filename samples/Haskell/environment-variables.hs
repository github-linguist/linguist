import System.Environment
main = do getEnv "HOME" >>= print  -- get env var
          getEnvironment >>= print -- get the entire environment as a list of (key, value) pairs
