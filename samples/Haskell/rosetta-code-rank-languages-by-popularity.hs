import Network.Browser
import Network.HTTP
import Network.URI
import Data.List
import Data.Maybe
import Text.XML.Light
import Control.Arrow
import Data.Ord

getRespons url = do
    rsp <- Network.Browser.browse $ do
      setAllowRedirects True
      setOutHandler $ const (return ())     -- quiet
      request $ getRequest url
    return $ rspBody $ snd rsp


mostPopLang = do
  rsp <-getRespons $ "http://www.rosettacode.org/w/api.php?action=query&list=" ++
		    "categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=xml"
  mbrs <- getRespons "http://www.rosettacode.org/w/index.php?title=Special:Categories&limit=5000"
  let xmls = onlyElems $ parseXML rsp
      langs = concatMap (map ((\\"Category:"). fromJust.findAttr (unqual "title")). filterElementsName (== unqual "cm")) xmls

  let catMbr = second (read.takeWhile(/=' '). drop 6). break (=='<'). drop 1. dropWhile(/='>') . drop 5
      catNmbs :: [(String, Int)]
      catNmbs = map catMbr $ filter (isPrefixOf "<li>") $ lines mbrs
      printFmt (n,(l,m)) = putStrLn $ take 6 (show n ++ ".     ") ++ (show m) ++ "  " ++ l
      toMaybe (a,b) =
	case b of
	  Just x -> Just (a,x)
	  _ -> Nothing

  mapM_ printFmt $  zip [1..] $ sortBy (flip (comparing snd))
    $ mapMaybe (toMaybe. (id &&& flip lookup catNmbs)) langs
