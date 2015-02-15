import Control.Monad
import Data.Time
import System.Locale

format1 :: FormatTime t => t -> String
format1 = formatTime defaultTimeLocale "%Y-%m-%e"

format2 :: FormatTime t => t -> String
format2 = formatTime defaultTimeLocale "%A, %B %d, %Y"

main = do
    t <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
    mapM_ putStrLn [format1 t, format2 t]
