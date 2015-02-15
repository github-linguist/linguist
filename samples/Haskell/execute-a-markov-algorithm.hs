import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment (getArgs)

main = do
   args <- getArgs
   unless (length args == 1) $
       fail "Please provide exactly one source file as an argument."
   let sourcePath = head args
   source <- readFile sourcePath
   input <- getContents
   case parse markovParser sourcePath source of
       Right rules -> putStr $ runMarkov rules input
       Left  err   -> hPutStrLn stderr $ "Parse error at " ++ show err

data Rule = Rule
   {from :: String, terminating :: Bool, to :: String}

markovParser :: Parser [Rule]
markovParser = liftM catMaybes $
    (comment <|> rule) `sepEndBy` many1 newline
  where comment = char '#' >> skipMany nonnl >> return Nothing
        rule = liftM Just $ liftM3 Rule
            (manyTill (nonnl <?> "pattern character") $ try arrow)
            (succeeds $ char '.')
            (many nonnl)
        arrow = ws >> string "->" >> ws <?> "whitespace-delimited arrow"
        nonnl = noneOf "\n"
        ws = many1 $ oneOf " \t"
        succeeds p = option False $ p >> return True

runMarkov :: [Rule] -> String -> String
runMarkov rules s = f rules s
  where f []                              s = s
        f (Rule from terminating to : rs) s = g "" s
          where g _      ""    = f rs s
                g before ahead@(a : as) = if from `isPrefixOf` ahead
                  then let new = reverse before ++ to ++ drop (length from) ahead
                       in if terminating then new else f rules new
                  else g (a : before) as
