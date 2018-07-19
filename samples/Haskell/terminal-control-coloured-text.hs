#!/usr/bin/runhaskell

import System.Console.ANSI

colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> String -> IO ()
colorStrLn fgi fg bgi bg str = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
  putStr str
  setSGR []
  putStrLn ""

main = do
  colorStrLn Vivid White Vivid Red "This is red on white."
  colorStrLn Vivid White Dull Blue "This is white on blue."
  colorStrLn Vivid Green Dull Black "This is green on black."
  colorStrLn Vivid Yellow Dull Black "This is yellow on black."
  colorStrLn Dull Black Vivid Blue "This is black on light blue."
