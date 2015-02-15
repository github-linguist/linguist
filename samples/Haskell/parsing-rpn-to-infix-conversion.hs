data Expression = Const String | Exp Expression String Expression

precedence :: Expression -> Int
precedence (Const _) = 5
precedence (Exp _ op _)
	| op `elem` ["^"]     = 4
	| op `elem` ["*","/"] = 3
	| op `elem` ["+","-"] = 2
	| otherwise = 0

leftAssoc :: Expression -> Bool
leftAssoc (Const _) = False
leftAssoc (Exp _ op _) = op `notElem` ["^","*","+"]

rightAssoc :: Expression -> Bool
rightAssoc (Const _) = False
rightAssoc (Exp _ op _) = op `elem` ["^"]

instance Show Expression where
	show (Const x) = x
	show exp@(Exp l op r) = left++" "++op++" "++right
		where left  = if leftNeedParen then "( "++(show l)++" )" else show l
		      right = if rightNeedParen the "( "++(show r)++" )" else show r
		      leftNeedParen = (leftPrec < opPrec) || ((leftPrec == opPrec) && (rightAssoc exp))
		      rightNeedParen = (rightPrec < opPrec) || ((rightPrec == opPrec) && (leftAssoc exp))
		      leftPrec  = precedence l
		      rightPrec = precedence r
		      opPrec    = precedence exp

buildExp :: [Expression] -> String -> [Expression]
buildExp stack x
	| not.isOp $ x = Const x : stack
	| otherwise    = Exp l x r : rest
		where r:l:rest = stack
		      isOp = (`elem` ["^","*","/","+","-"])

main = do
	contents <- getContents
	mapM_ (print.head.(foldl buildExp []).words) (lines contents)
