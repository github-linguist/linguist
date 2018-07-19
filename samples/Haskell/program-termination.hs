import Control.Monad
import System.Exit

when problem do
    exitWith ExitSuccess                    -- success
    exitWith (ExitFailure integerErrorCode) -- some failure with code
    exitSuccess                             -- success; in GHC 6.10+
    exitFailure                             -- generic failure
