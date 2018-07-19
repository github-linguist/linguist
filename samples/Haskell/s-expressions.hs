import Text.ParserCombinators.Parsec ((<|>), (<?>), many, many1, char, try, parse, sepBy, choice)
import Text.ParserCombinators.Parsec.Char (noneOf)
import Text.ParserCombinators.Parsec.Token (integer, float, whiteSpace, stringLiteral, makeTokenParser)
import Text.ParserCombinators.Parsec.Language (haskellDef)


data Val = Int Integer
         | Float Double
         | String String
         | Symbol String
         | List [Val] deriving (Eq, Show)

lexer = makeTokenParser haskellDef

tInteger = (integer lexer) >>= (return . Int) <?> "integer"

tFloat = (float lexer) >>= (return . Float) <?> "floating point number"

tString = (stringLiteral lexer) >>= (return . String) <?> "string"

tSymbol = (many1 $ noneOf "()\" \t\n\r") >>= (return . Symbol) <?> "symbol"

tAtom = choice [try tFloat, try tInteger, tSymbol, tString] <?> "atomic expression"

tExpr = do
    whiteSpace lexer
    expr <- tList <|> tAtom
    whiteSpace lexer
    return expr
    <?> "expression"

tList = do
    char '('
    list <- many tExpr
    char ')'
    return $ List list
    <?> "list"

tProg = many tExpr <?> "program"

p ex = case parse tProg "" ex of
              Right x -> putStrLn $  unwords $ map show x
              Left err -> print err

main = do
    let expr = "((data \"quoted data\" 123 4.5)\n  (data (!@# (4.5) \"(more\" \"data)\")))"
    putStrLn $ "The input:\n" ++ expr ++ "\n"
    putStr "Parsed as:\n"
    p expr
