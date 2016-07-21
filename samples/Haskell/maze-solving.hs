#!/usr/bin/runhaskell

import Data.Maybe

-- given two points, returns the average of them
average :: (Int, Int) -> (Int, Int) -> (Int, Int)
average (x, y) (x', y') = ((x + x') `div` 2, (y + y') `div` 2)

-- given a maze and a tuple of position and wall position, returns
-- true if the wall position is not blocked (first position is unused)
notBlocked :: [String] -> ((Int, Int), (Int, Int)) -> Bool
notBlocked maze (_, (x, y)) = ' ' == (maze !! y) !! x

-- given a list, a position, and an element, returns a new list
-- with the new element substituted at the position
-- (it seems such a function should exist in the standard library;
-- I must be missing it)
substitute :: [a] -> Int -> a -> [a]
substitute orig pos el =
  let (before, after) = splitAt pos orig
  in before ++ [el] ++ tail after

-- given a maze and a position, draw a '*' at that position in the maze
draw :: [String] -> (Int, Int) -> [String]
draw maze (x,y) = substitute maze y $ substitute row x '*'
  where row = maze !! y

-- given a maze, a previous position, and a list of tuples of potential
-- new positions and their wall positions, returns the solved maze, or
-- None if it cannot be solved
tryMoves :: [String] -> (Int, Int) -> [((Int, Int), (Int, Int))] -> Maybe [String]
tryMoves _ _ [] = Nothing
tryMoves maze prevPos ((newPos,wallPos):more) =
  case solve' maze newPos prevPos
       of Nothing -> tryMoves maze prevPos more
          Just maze' -> Just $ foldl draw maze' [newPos, wallPos]

-- given a maze, a new position, and a previous position, returns
-- the solved maze, or None if it cannot be solved
-- (assumes goal is upper-left corner of maze)
solve' :: [String] -> (Int, Int) -> (Int, Int) -> Maybe [String]
solve' maze (2, 1) _ = Just maze
solve' maze pos@(x, y) prevPos =
  let newPositions = [(x, y - 2), (x + 4, y), (x, y + 2), (x - 4, y)]
      notPrev pos' = pos' /= prevPos
      newPositions' = filter notPrev newPositions
      wallPositions = map (average pos) newPositions'
      zipped = zip newPositions' wallPositions
      legalMoves = filter (notBlocked maze) zipped
  in tryMoves maze pos legalMoves

-- given a maze, returns a solved maze, or None if it cannot be solved
-- (starts at lower right corner and goes to upper left corner)
solve :: [String] -> Maybe [String]
solve maze = solve' (draw maze start) start (-1, -1)
  where startx = length (head maze) - 3
        starty = length maze - 2
        start = (startx, starty)

-- takes unsolved maze on standard input, prints solved maze on standard output
main = interact main'
  where main' x = unlines $ fromMaybe ["can't solve"] $ solve $ lines x
