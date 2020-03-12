{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Puzzles where

import Data.Holmes
import Tower

{-
  Sample puzzles
-}

config3 :: Config Holmes (Defined (Cell Int))
config3 = definedConfig (Board 3)

{-

  (.) 2  .
   .  .  .
   .  .  .


   1  2  3
   3  1  2
   2  3  1

-}

-- restricted to Defined because `givens` uses `pure` to create single values to test against
puzzle3 :: forall m. MonadCell m => [Prop m (Defined (Cell Int))] -> Prop m (Defined Bool)
puzzle3 cells =
  and'
    [ and' (map (\(c, v) -> givenNum (toIndex board c) v cells) givens),
      and' (map (\i -> isTower i cells) towerIndexes),
      and' (map (\i -> not' $ isTower i cells) nonTowerIndexes),
      latinSquare' board cells,
      and' (map towerConstraint' towers)
    ]
  where
    board = Board 3
    givens = [(Coord 1 0, 2)]
    towers = [Coord 0 0]
    towerIndexes = map (toIndex board) towers
    nonTowerIndexes = filter (not . flip elem towerIndexes) (boardIndexes board)
    towerConstraint' coord = towerConstraint board coord cells

puzzle1 cells = towerConstraint (Board 1) (Coord 0 0) cells

puzzle2 cells =
  and'
    [ latinSquare' (Board 2) cells,
      towerConstraint (Board 2) (Coord 0 0) cells
    ]

-- let's start with our 4x4 puzzle:
{-
  .OO.
  ...O
  ....
  ....
-}

config4 :: Config Holmes (Defined (Cell Int))
config4 = definedConfig (Board 4)

puzzle4 ::
  ( Mapping f c,
    c (Cell v),
    c v,
    c Bool,
    EqR (f v) (f Bool),
    Eq v,
    OrdR (f v) (f Bool),
    Num v,
    Num (f v),
    SumR (f v),
    MonadCell m
  ) =>
  [Prop m (f (Cell v))] ->
  Prop m (f Bool)
puzzle4 cells =
  and'
    [ and' (map (\i -> isTower i cells) towerIndexes),
      and' (map (\i -> not' $ isTower i cells) nonTowerIndexes),
      latinSquare' board cells,
      and' (map towerConstraint' towers)
    ]
  where
    board = Board 4
    towers = [Coord 0 1, Coord 0 2, Coord 1 3]
    towerIndexes = map (toIndex board) towers
    nonTowerIndexes = filter (not . flip elem towerIndexes) (boardIndexes board)
    towerConstraint' coord = towerConstraint board coord cells

{-
  Example puzzle from the GP instructions

    4  .  .  .  .
    .  1 (4) .  .
    . (3) . (.) .
    .  . (.) 1  .
    .  .  .  .  2
-}

config5 :: Config Holmes (Defined (Cell Int))
config5 = definedConfig (Board 5)

puzzle5 :: forall m. MonadCell m => [Prop m (Defined (Cell Int))] -> Prop m (Defined Bool)
puzzle5 cells =
  and'
    [ and' (map (\(c, v) -> givenNum (toIndex board c) v cells) givens),
      and' (map (\i -> isTower i cells) towerIndexes),
      and' (map (\i -> not' $ isTower i cells) nonTowerIndexes),
      latinSquare' board cells,
      and' (map towerConstraint' towers)
    ]
  where
    board = Board 5
    givens = [(Coord 0 0, 4), (Coord 1 1, 1), (Coord 1 2, 4), (Coord 2 1, 3), (Coord 3 3, 1), (Coord 4 4, 2)]
    towers = [Coord 1 2, Coord 2 1, Coord 2 3, Coord 3 2]
    towerIndexes = map (toIndex board) towers
    nonTowerIndexes = filter (not . flip elem towerIndexes) (boardIndexes board)
    towerConstraint' coord = towerConstraint board coord cells
