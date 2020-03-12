module Main where

import Data.Holmes
import Puzzles
import Tower

main :: IO ()
main = do
  putStrLn "solving 4x4"
  solve (Board 4) config4 puzzle4
  putStrLn "solving 5x5"
  solve (Board 5) config5 puzzle5
