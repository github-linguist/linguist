module Prelude.Char

import Builtins

isUpper : Char -> Bool
isUpper x = x >= 'A' && x <= 'Z'

isLower : Char -> Bool
isLower x = x >= 'a' && x <= 'z'

isAlpha : Char -> Bool
isAlpha x = isUpper x || isLower x

isDigit : Char -> Bool
isDigit x = (x >= '0' && x <= '9')

isAlphaNum : Char -> Bool
isAlphaNum x = isDigit x || isAlpha x

isSpace : Char -> Bool
isSpace x = x == ' '  || x == '\t' || x == '\r' ||
            x == '\n' || x == '\f' || x == '\v' ||
            x == '\xa0'

isNL : Char -> Bool
isNL x = x == '\r' || x == '\n'

toUpper : Char -> Char
toUpper x = if (isLower x)
               then (prim__intToChar (prim__charToInt x - 32))
               else x

toLower : Char -> Char
toLower x = if (isUpper x)
               then (prim__intToChar (prim__charToInt x + 32))
               else x

isHexDigit : Char -> Bool
isHexDigit x = elem (toUpper x) hexChars where
  hexChars : List Char
  hexChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
              'A', 'B', 'C', 'D', 'E', 'F']
