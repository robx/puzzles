{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Puzzles where

import Data.Holmes
import Tower

{-
  Sample puzzles
-}

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
