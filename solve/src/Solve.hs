module Solve
    ( someFunc
    ) where

{-
  A puzzle is given by:

a : 

-- all possible "moves" to make, grouped by location
moves : Grid -> [(Location, [Move])]

-- modify the grid by making a move
move : (Location, Move) -> Grid -> Grid

-- check whether a grid is "obviously" contradictory
broken : Grid -> Bool

moveMaybe : (Location, Move) -> Grid -> Maybe Grid
moveMaybe (l, m) g =
    let g' = move (l, m) g
    in
      if broken g' then Nothing else Just g

-- given a grid, find the depth of the solve tree
-- for each location
step : Grid -> [(Location, Int)]

depth : Grid -> Location -> Int

depthCapped : Int -> Grid -> Location -> Int

someFunc :: IO ()
someFunc = putStrLn "someFunc"
