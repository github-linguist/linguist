import System.Random

data Choice = Rock | Paper | Scissors
 deriving (Show, Eq)


beats Paper Rock = True
beats Scissors Paper = True
beats Rock Scissors = True
beats _ _ = False

genrps (r,p,s) = fmap rps rand
  where rps x | x <= s    = Rock
              | x <= s+r  = Paper
              | otherwise = Scissors
        rand = randomRIO (1,r+p+s) :: IO Int

getrps = fmap rps getLine
  where rps "scissors" = Scissors
        rps "rock" = Rock
        rps "paper" = Paper
        rps _ = error "invalid input"

game (r,p,s) = do putStrLn "rock, paper or scissors?"
                  h <- getrps
                  c <- genrps (r,p,s)
                  putStrLn ("Player: " ++ show h ++ " Computer: " ++ show c)
                  putStrLn (if beats h c then "player wins\n"
                           else if beats c h then "player loses\n"
                           else "draw\n")
                  let rr = if h == Rock then r+1 else r
                      pp = if h == Paper then p+1 else p
                      ss = if h == Scissors then s+1 else s
                  game (rr,pp,ss)

main = game (1,1,1)
