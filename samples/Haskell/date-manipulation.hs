import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale

main = print t2
  where t1 = readTime defaultTimeLocale
            "%B %e %Y %l:%M%P %Z"
            "March 7 2009 7:30pm EST"
        t2 = posixSecondsToUTCTime $ 12*60*60 + utcTimeToPOSIXSeconds t1
