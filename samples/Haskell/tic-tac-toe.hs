module Main where

import System.Random
import Data.List (intercalate, find, minimumBy)
import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Monad (guard)
import Data.Ord (comparing)

-- check if there is a horizontal, vertical or diagonal line of
-- X or O
tictactoe :: String -> Bool
tictactoe a = tictactoeFor 'X' a /= tictactoeFor 'O' a

-- check if there is a horizontal, vertical or diagonal line
-- for the given player "n"
tictactoeFor :: Char -> String -> Bool
tictactoeFor n [a,b,c,d,e,f,g,h,i] =
    [n,n,n] `elem` [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                    [b,e,h],[c,f,i],[a,e,i],[c,e,g]]

-- empty game board
start :: String
start = "         "

-- check if there is an X or an O at the given position
isPossible :: Int -> String -> Bool
isPossible n game = (game !! n) `notElem` "XO"

-- try to place an X or an O at a given position.
-- "Right" + modified board means success, "Left" + unmodified board
-- means failure
place :: Int -> Char -> String -> Either String String
place i c game =
    if isPossible i game
    then Right $ take i game ++ [c] ++ drop (i + 1) game
    else Left game

-- COMPUTER AI
-- get the number of movements, starting from a given non-empty board
-- and a position for the next movement, until the specified player
-- wins or no movement is possible
-- the positions are chosen sequentially, so there's not much
-- intelligence here anyway
developGame :: Bool -> Int -> Int -> Char -> String -> (Int, Char, String)
developGame iterateMore moves i player game
    | i > 8 =
        -- if i arrives to the last position, iterate again from 0
        -- but do it only once
        if iterateMore
        then developGame False moves 0 player game
        -- draw game (after one iteration, still no winning moves)
        else (moves, player, game)
        -- draw game (game board full) or a win for the player
    | moves == 9 || tictactoeFor player game = (moves, player, game)
        -- make a move, if possible, and continue playing
    | otherwise = case place i otherPlayer game of
        -- position i is not empty. try with the next position
        Left _ -> developGame iterateMore moves (i + 1)
                    otherPlayer game
        -- position i was empty, so it was a valid move.
        -- change the player and make a new move, starting at pos 0
        Right newGame -> developGame iterateMore (moves + 1) 0
                    otherPlayer newGame
        where
            otherPlayer = changePlayer player

-- COMPUTER AI
-- starting from a given non-empty board, try to guess which position
-- could lead the player to the fastest victory.
bestMoveFor :: Char -> String -> Int
bestMoveFor player game = bestMove
    where
        -- drive the game to its end for each starting position
        continuations = [ (x, developGame True 0 x player game) |
            x <- [0..8] ]
        -- compare the number of moves of the game and take the
        -- shortest one
        move (_, (m, _, _)) = m
        (bestMove, _) = minimumBy (comparing move) continuations

-- canBlock checks if the opponent has two pieces in a row and the
-- other cell in the row is empty, and places the player's piece there,
-- blocking the opponent
canBlock :: Char -> String -> Maybe Int
canBlock p [a,b,c,d,e,f,g,h,i] =
    listToMaybe $ mapMaybe blockable [[a,b,c],[d,e,f],[g,h,i],[a,d,g],
                                      [b,e,h],[c,f,i],[a,e,i],[c,e,g]]
    where
        blockable xs = do
          guard $ length (filter (== otherPlayer) xs) == 2
          x <- find (`elem` "123456789") xs
          return $ digitToInt x
        otherPlayer = changePlayer p

-- format a game board for on-screen printing
showGame :: String -> String
showGame [a,b,c,d,e,f,g,h,i] =
    topBottom ++
    "|    | 1 | 2 | 3 |\n" ++
    topBottom ++
    row "0" [[a],[b],[c]] ++
    row "3" [[d],[e],[f]] ++
    row "6" [[g],[h],[i]]
    where
        topBottom = "+----+---+---+---+\n"
        row n x = "| " ++ n ++ "+ | " ++
            intercalate " | " x ++ " |\n" ++ topBottom

-- ask the user to press a numeric key and convert it to an int
enterNumber :: IO Int
enterNumber = do
    c <- getChar
    if c `elem` "123456789"
    then do
        putStrLn ""
        return $ digitToInt c
    else do
        putStrLn "\nPlease enter a digit!"
        enterNumber

-- a human player's turn: get the number of pieces put on the board,
-- the next piece to be put (X or O) and a game board, and return
-- a new game state, checking if the piece can be placed on the board.
-- if it can't, make the user try again.
turn :: (Int, Char, String) -> IO (Int, Char, String)
turn (count, player, game) = do
    putStr $ "Please tell me where you want to put an " ++
        [player] ++ ": "
    pos <- enterNumber
    case place (pos - 1) player game of
        Left oldGame -> do
            putStrLn "That place is already taken!\n"
            turn (count, player, oldGame)
        Right newGame ->
            return (count + 1, changePlayer player, newGame)

-- alternate between X and O players
changePlayer :: Char -> Char
changePlayer 'O' = 'X'
changePlayer 'X' = 'O'

-- COMPUTER AI
-- make an automatic turn, placing an X or an O game board.
-- the first movement is always random.
-- first, the computer looks for two pieces of his opponent in a row
-- and tries to block.
-- otherwise, it tries to guess the best position for the next movement.
-- as a least ressource, it places a piece randomly.
autoTurn :: Bool -> (Int, Char, String) -> IO (Int, Char, String)
autoTurn forceRandom (count, player, game) = do
    -- try a random position 'cause everything else failed
    -- count == 0 overrides the value of forceRandom
    i <- if count == 0 || forceRandom
            then randomRIO (0,8)
            else return $
                case canBlock player game of
                    -- opponent can't be blocked. try to guess
                    -- the best movement
                    Nothing -> bestMoveFor player game
                    -- opponent can be blocked, so just do it!
                    Just blockPos -> blockPos
    -- if trying to place a piece at a calculated position doesn't work,
    -- just try again with a random value
    case place i player game of
        Left oldGame -> autoTurn True (count, player, oldGame)
        Right newGame -> do
            putStrLn $ "It's player " ++ [player] ++ "'s turn."
            return (count + 1, changePlayer player, newGame)

-- play a game until someone wins or the board becomes full.
-- depending on the value of the variable "auto", ask the user(s) to
-- put some pieces on the board or do it automatically
play :: Int -> (Int, Char, String) -> IO ()
play auto cpg@(_, player, game) = do
    newcpg@(newCount, newPlayer, newGame) <- case auto of
        -- if both players are human, always ask them
        0 -> turn cpg
        -- if both players are computer, always play auto
        1 -> autoTurn False cpg
        -- X is computer, O is human
        2 -> if player == 'X' then autoTurn False cpg else turn cpg
        -- X is human, O is computer
        3 -> if player == 'O' then autoTurn False cpg else turn cpg
    putStrLn $ "\n" ++ showGame newGame
    if tictactoe newGame
    then putStrLn $ "Player " ++ [changePlayer newPlayer] ++ " wins!\n"
    else
        if newCount == 9
        then putStrLn "Draw!\n"
        else play auto newcpg

-- main program: greet the user, ask for a game type, ask for the
-- player that'll start the game, and play the game beginning with an
-- empty board
main :: IO ()
main = do
    a <- getArgs
    if null a
    then usage
    else do
        let option = head a
        if option `elem` ["0","1","2","3"]
        then do
            putStrLn $ "\n" ++ showGame start
            let m = read option :: Int
            play m (0, 'X', start)
        else usage

usage :: IO ()
usage = do
    putStrLn "TIC-TAC-TOE GAME\n================\n"
    putStrLn "How do you want to play?"
    putStrLn "Run the program with one of the following options."
    putStrLn "0 : both players are human"
    putStrLn "1 : both players are computer"
    putStrLn "2 : player X is computer and player O is human"
    putStrLn "3 : player X is human and player O is computer"
    putStrLn "Player X always begins."
