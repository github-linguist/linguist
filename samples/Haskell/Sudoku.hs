module Sudoku
(
  Sudoku,
  solve,
  isSolved,
  pPrint
) where

import Data.Maybe
import Data.List
import Data.List.Split

type Sudoku = [Int]

solve :: Sudoku -> Maybe Sudoku
solve sudoku
  | isSolved sudoku = Just sudoku
  | otherwise = do
    index <- elemIndex 0 sudoku
    let sudokus = [nextTest sudoku index i | i <- [1..9],
                                  checkRow (nextTest sudoku index i) index,
                                  checkColumn (nextTest sudoku index i) index,
                                  checkBox (nextTest sudoku index i) index]
    listToMaybe $ mapMaybe solve sudokus
  where nextTest sudoku index i = take index sudoku ++ [i] ++ drop (index+1) sudoku
        checkRow sudoku index = (length $ getRow sudoku index) == (length $ nub $ getRow sudoku index)
        checkColumn sudoku index = (length $ getColumn sudoku index) == (length $ nub $ getColumn sudoku index)
        checkBox sudoku index = (length $ getBox sudoku index) == (length $ nub $ getBox sudoku index)
        getRow sudoku index = filter (/=0) $ (chunksOf 9 sudoku) !! (quot index 9)
        getColumn sudoku index = filter (/=0) $ (transpose $ chunksOf 9 sudoku) !! (mod index 9)
        getBox sudoku index = filter (/=0) $ (map concat $ concatMap transpose $ chunksOf 3 $ map (chunksOf 3) $ chunksOf 9 sudoku)
                                                                                !! (3 * (quot index 27) + (quot (mod index 9) 3))

isSolved :: Sudoku -> Bool
isSolved sudoku
  | product sudoku == 0 = False
  | map (length . nub) sudokuRows /= map length sudokuRows = False
  | map (length . nub) sudokuColumns /= map length sudokuColumns = False
  | map (length . nub) sudokuBoxes /= map length sudokuBoxes = False
  | otherwise = True
  where sudokuRows = chunksOf 9 sudoku
        sudokuColumns = transpose sudokuRows
        sudokuBoxes = map concat $ concatMap transpose $ chunksOf 3 $ map (chunksOf 3) $ chunksOf 9 sudoku

pPrint :: Sudoku -> String
pPrint sudoku = intercalate "\n" $ map (intercalate " " . map show) $ chunksOf 9 sudoku