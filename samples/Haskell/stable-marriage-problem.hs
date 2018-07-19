import Data.List
import Control.Monad
import Control.Arrow
import Data.Maybe

mp = map ((head &&& tail). splitNames)
   ["abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay",
    "bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay",
    "col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan",
    "dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi",
    "ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay",
    "fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay",
    "gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay",
    "hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee",
    "ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve",
    "jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope"]

fp = map ((head &&& tail). splitNames)
    ["abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal",
     "bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal",
     "cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon",
     "dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed",
     "eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob",
     "fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal",
     "gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian",
     "hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred",
     "ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan",
     "jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan"]

splitNames = map (takeWhile(`notElem`",:")). words

pref x y xs = fromJust (elemIndex x xs) < fromJust (elemIndex y xs)

task ms fs = do
 let
  jos = fst $ unzip ms

  runGS  es js ms = do
    let (m:js') = js
	(v:vm') = case lookup m ms of
		    Just xs -> xs
		    _ -> []
	vv = fromJust $ lookup v fs
	m2 = case lookup v es of
		Just e -> e
		_ -> ""
	ms1 = insert (m,vm') $ delete (m,v:vm') ms

    if null js then do
	putStrLn ""
	putStrLn "=== Couples ==="
	return es

      else if null m2 then
	do putStrLn $ v ++ " with " ++ m
	   runGS ( insert (v,m) es ) js' ms1
	
      else if pref m m2 vv then
	do putStrLn $ v ++ " dumped " ++ m2 ++ " for " ++ m
	   runGS ( insert (v,m) $ delete (v,m2) es ) (if not $ null vm' then js'++[m2] else js') ms1

      else runGS es (if not $ null js' then js'++[m] else js') ms1

 cs <- runGS [] jos ms

 mapM_ (\(f,m) ->  putStrLn $ f ++ " with " ++ m ) cs
 putStrLn ""
 checkStab cs

 putStrLn ""
 putStrLn "Introducing error: "
 let [r1@(a,b), r2@(p,q)] = take 2 cs
     r3 = (a,q)
     r4 = (p,b)
     errcs =  insert r4. insert r3. delete r2 $ delete r1 cs
 putStrLn $ "\tSwapping partners of " ++ a ++ " and " ++ p
 putStrLn $ (\((a,b),(p,q)) -> "\t" ++ a ++ " is now with " ++ b ++ " and " ++ p ++ " with " ++ q) (r3,r4)
 putStrLn ""
 checkStab errcs

checkStab es = do
  let
    fmt (a,b,c,d) =  a ++ " and " ++ b ++ " like each other better than their current partners " ++ c ++ " and " ++ d
    ies = uncurry(flip zip) $ unzip es  -- es = [(fem,m)]  & ies = [(m,fem)]
    slb = map (\(f,m)-> (f,m, map (id &&& fromJust. flip lookup ies). fst.break(==m). fromJust $ lookup f fp) ) es
    hlb = map (\(f,m)-> (m,f, map (id &&& fromJust. flip lookup es ). fst.break(==f). fromJust $ lookup m mp) ) es
    tslb = concatMap (filter snd. (\(f,m,ls) ->
				map (\(m2,f2) ->
				      ((f,m2,f2,m), pref f f2 $ fromJust $ lookup m2 mp)) ls)) slb
    thlb = concatMap (filter snd. (\(m,f,ls) ->
				map (\(f2,m2) ->
				      ((m,f2,m2,f), pref m m2 $ fromJust $ lookup f2 fp)) ls)) hlb
    res = tslb ++ thlb

  if not $ null res then do
    putStrLn "Marriages are unstable, e.g.:"
    putStrLn.fmt.fst $ head res

   else putStrLn "Marriages are stable"
