data Color = Black | White
  deriving (Read, Show, Enum, Eq, Ord)

putCell c = putStr (case c of Black -> "#"
                              White -> ".")

toggle :: Color -> Color
toggle color = toEnum $ 1 - fromEnum color


data Dir = East | North | West | South
  deriving (Read, Show, Enum, Eq, Ord)

turnLeft South = East
turnLeft dir = succ dir

turnRight East = South
turnRight dir = pred dir

data Pos = Pos { x :: Int,  y :: Int }
  deriving (Read)

instance Show Pos where
  show p@(Pos x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"

-- Return the new position after moving one unit in the given direction
moveOne pos@(Pos x y) dir =
  case dir of
    East  -> Pos (x+1) y
    South -> Pos x (y+1)
    West  -> Pos (x-1) y
    North -> Pos x (y-1)

-- Grid is just a list of lists
type Grid = [[Color]]

colorAt g p@(Pos x y) = (g !! y) !! x

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

toggleCell g p@(Pos x y) =
  let newVal = toggle $ colorAt g p
  in replaceNth y (replaceNth x newVal (g !! y)) g

printRow r = do { mapM_ putCell r ; putStrLn "" }

printGrid g = mapM_ printRow g


data State = State { move :: Int, pos :: Pos, dir :: Dir, grid :: Grid }

printState s = do {
  putStrLn $ show s;
  printGrid $ grid s
}

instance Show State where
   show s@(State m p@(Pos x y) d g) =
     "Move: " ++ (show m) ++  " Pos: " ++ (show p) ++ " Dir: " ++ (show d)

nextState s@(State m p@(Pos x y) d g) =
  let color = colorAt g p
      new_d = case color of White -> (turnRight d)
                            Black -> (turnLeft d)
      new_m = m + 1
      new_p = moveOne p new_d
      new_g = toggleCell g p
  in State new_m new_p new_d new_g

inRange size s@(State m p@(Pos x y) d g) =
  x >= 0 && x < size && y >= 0 && y < size

initialState size = (State 0 (Pos (size`div`2) (size`div`2)) East [ [ White | x <- [1..size] ] | y <- [1..size] ])

--- main
size = 100
allStates = initialState size : [nextState s | s <- allStates]

main = printState $ last $ takeWhile (inRange size) allStates
