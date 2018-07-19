import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp

expr = buildExpressionParser table factor

table = [[op "*" (Mul) AssocLeft, op "/" (Div) AssocLeft]
        ,[op "+" (Add) AssocLeft, op "-" (Sub) AssocLeft]]
        where op s f assoc = Infix (do string s; return f) assoc

factor =  do char '(' ; x <- expr ; char ')'
             return x
      <|> do ds <- many1 digit
             return $ Num (read ds)

evaluate (Num x) = fromIntegral x
evaluate (Add a b) = (evaluate a)   +   (evaluate b)
evaluate (Sub a b) = (evaluate a)   -   (evaluate b)
evaluate (Mul a b) = (evaluate a)   *   (evaluate b)
evaluate (Div a b) = (evaluate a) `div` (evaluate b)

solution exp = case parse expr [] exp of
                 Right expr -> evaluate expr
                 Left _ -> error "Did not parse"
