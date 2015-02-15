import Network.Browser
import Network.HTTP
import Network.URI
import Data.List
import Data.Maybe
import Text.XML.Light
import Control.Arrow
import Data.Char

getRespons url = do
  rsp <- Network.Browser.browse $ do
    setAllowRedirects True
    setOutHandler $ const (return ())   -- quiet
    request $ getRequest url
  return $ rspBody $ snd rsp

replaceWithSpace c = (\x -> if c==x then ' ' else x)

encl = chr 34

unimpTasks lang = do
  allTasks <- getRespons "http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"
  impl <-  getRespons ( "http://rosettacode.org/json/isb/" ++ lang ++ ".json")
  let langxx = map (map(replaceWithSpace '_')) $ filter (/=",") $ words $ map (replaceWithSpace encl ) $ init $ drop 1 impl
      xml = onlyElems $ parseXML allTasks
      allxx = concatMap (map (fromJust.findAttr (unqual "title")). filterElementsName (== unqual "cm")) xml
  mapM_ putStrLn $ sort $ allxx \\ langxx
