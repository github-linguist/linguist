import Control.Monad

-- In Haskell strings are just lists (of characters), so we can use the function
-- 'null', which applies to all lists.  We don't want to use the length, since
-- Haskell allows infinite lists.

main = do
  let s = ""
  when (null s) (putStrLn "Empty.")
  when (not $ null s) (putStrLn "Not empty.")
