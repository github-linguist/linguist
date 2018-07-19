import Test.QuickCheck

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

{- There is no built-in definition of how to generate random characters;
   here we just specify ASCII characters. Generating strings then automatically
   follows from the definition of String as list of Char. -}
instance Arbitrary Char where
  arbitrary = choose ('\32', '\127')

--                                            /------------------------- the randomly-generated parameters
--                                            |      /------------------ the constraint on the test values
--                                            |      |                /- the condition which should be true
--                                            v      v                v
main = do
  putStr "Even palindromes: " >> quickCheck (\s   ->                  isPalindrome (s ++ reverse s))
  putStr "Odd palindromes:  " >> quickCheck (\s   -> not (null s) ==> isPalindrome (s ++ (tail.reverse) s))
  putStr "Non-palindromes:  " >> quickCheck (\i s -> not (null s) && 0 <= i && i < length s && i*2 /= length s
                                                                  ==> not (isPalindrome (take i s ++ "•" ++ drop i s)))
