import Data.List
import Control.Arrow
import Rosetta.Knuthshuffle

numberRevGame = do
  let goal = [1..9]

      shuffle xs =
	if xs /= goal then return xs
		    else shuffle =<< knuthShuffle xs

      prefixFlipAt k = uncurry (++). first reverse. splitAt k

      prompt r ry = do
	 putStr $ show r ++ ". " ++ concatMap (flip (++) " ". show) ry
		   ++ " How many to flip? "
	 answ <- getLine
	 let n = read answ
	 if n<10 && 0<n then return n
	    else do
	      putStrLn "Error. The number should be between 0 and 10. Try again"
	      prompt r ry

      playNRG r nrs =
	if nrs == goal then do
	  putStrLn $ "The answer is: " ++ concatMap (flip (++) " ". show) nrs
	  putStrLn $ "It took you " ++ show r ++ " attempts to sort the numbers."
	  putStrLn ""
		else do
		  answ <- prompt r nrs
		  playNRG (succ r) (prefixFlipAt answ nrs)
	
  start <- shuffle goal

  playNRG 1 start
