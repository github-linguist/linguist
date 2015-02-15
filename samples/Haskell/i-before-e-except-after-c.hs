import Network.HTTP
import Text.Regex.TDFA
import Text.Printf

getWordList :: IO String
getWordList  =  do
    response  <-  simpleHTTP.getRequest$ url
    getResponseBody response
        where url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

main = do
    words <- getWordList
    putStrLn "Checking Rule 1: \"I before E when not preceded by C\"..."
    let numTrueRule1   =  matchCount (makeRegex "[^c]ie" :: Regex) words
        numFalseRule1  =  matchCount (makeRegex "[^c]ei" :: Regex) words
        rule1Plausible  =  numTrueRule1 > (2*numFalseRule1)
    printf "Rule 1 is correct for %d\n        incorrect for %d\n" numTrueRule1 numFalseRule1
    printf "*** Rule 1 is %splausible.\n" (if rule1Plausible then "" else "im")

    putStrLn "Checking Rule 2: \"E before I when preceded by C\"..."
    let numTrueRule2   =  matchCount (makeRegex "cei" :: Regex) words
        numFalseRule2  =  matchCount (makeRegex "cie" :: Regex) words
        rule2Plausible  =  numTrueRule2 > (2*numFalseRule2)
    printf "Rule 2 is correct for %d\n        incorrect for %d\n" numTrueRule2 numFalseRule2
    printf "*** Rule 2 is %splausible.\n" (if rule2Plausible then "" else "im")
