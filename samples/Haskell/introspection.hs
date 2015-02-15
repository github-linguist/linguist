import Data.Version
import Control.Monad
import System.Info

minGHCVersion = Version [6, 8] []

main = when (compilerName == "ghc" && compilerVersion < minGHCVersion) $
    fail "Compiler too old."
