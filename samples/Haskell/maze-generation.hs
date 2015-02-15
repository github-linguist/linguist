import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import System.Random

rand :: Random a => (a, a) -> STRef s StdGen -> ST s a
rand range gen = do
    (a, g) <- liftM (randomR range) $ readSTRef gen
    gen `writeSTRef` g
    return a

data Maze = Maze {rightWalls, belowWalls :: Array (Int, Int) Bool}

maze :: Int -> Int -> StdGen -> ST s Maze
maze width height gen = do
    visited <- mazeArray False
    rWalls <- mazeArray True
    bWalls <- mazeArray True
    gen <- newSTRef gen
    liftM2 (,) (rand (0, maxX) gen) (rand (0, maxY) gen) >>=
        visit gen visited rWalls bWalls
    liftM2 Maze (freeze rWalls) (freeze bWalls)
  where visit gen visited rWalls bWalls here = do
            writeArray visited here True
            let ns = neighbors here
            i <- rand (0, length ns - 1) gen
            forM_ (ns !! i : take i ns ++ drop (i + 1) ns) $ \there -> do
                seen <- readArray visited there
                unless seen $ do
                    removeWall here there
                    visit gen visited rWalls bWalls there
          where removeWall (x1, y1) (x2, y2) = writeArray
                    (if x1 == x2 then bWalls else rWalls)
                    (min x1 x2, min y1 y2)
                    False

        neighbors (x, y) =
            (if x == 0    then [] else [(x - 1, y    )]) ++
            (if x == maxX then [] else [(x + 1, y    )]) ++
            (if y == 0    then [] else [(x,     y - 1)]) ++
            (if y == maxY then [] else [(x,     y + 1)])

        maxX = width - 1
        maxY = height - 1

        mazeArray = newArray ((0, 0), (maxX, maxY))
            :: Bool -> ST s (STArray s (Int, Int) Bool)

printMaze :: Maze -> IO ()
printMaze (Maze rWalls bWalls) = do
    putStrLn $ '+' : (concat $ replicate (maxX + 1) "---+")
    forM_ [0 .. maxY] $ \y -> do
        putStr "|"
        forM_ [0 .. maxX] $ \x -> do
            putStr "   "
            putStr $ if rWalls ! (x, y) then "|" else " "
        putStrLn ""
        forM_ [0 .. maxX] $ \x -> do
            putStr "+"
            putStr $ if bWalls ! (x, y) then "---" else "   "
        putStrLn "+"
  where maxX = fst (snd $ bounds rWalls)
        maxY = snd (snd $ bounds rWalls)

main = getStdGen >>= stToIO . maze 11 8 >>= printMaze
