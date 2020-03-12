{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module Tower where

import Data.Hashable (Hashable)
import Data.Holmes
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Data.Propagator
import GHC.Generics (Generic)

{-
  General puzzle board things.
-}

newtype Board = Board {boardSize :: Int}

type Index = Int

boardIndexes :: Board -> [Index]
boardIndexes (Board n) = [0 .. n * n -1]

data Coord = Coord {row :: Int, col :: Int}

inBoard :: Board -> Coord -> Bool
inBoard (Board n) (Coord r c) = r >= 0 && r < n && c >= 0 && c < n

toIndex :: Board -> Coord -> Int
toIndex (Board n) (Coord r c) = r * n + c

fromIndex :: Board -> Int -> Coord
fromIndex (Board n) i =
  let (r, c) = i `divMod` n
   in Coord r c

rows :: Board -> [x] -> [[x]]
rows _ [] = []
rows b@(Board n) xs = take n xs : rows b (drop n xs)

cols :: Board -> [x] -> [[x]]
cols board = transpose . rows board

{-
  Encoding tower defence
-}

data Cell v = Cell {value :: v, tower :: Bool}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

instance Enum v => Enum (Cell v) where

  toEnum n = let (v, t) = n `divMod` 2 in Cell (toEnum v) (toEnum t)

  fromEnum (Cell v t) = 2 * (fromEnum v) + fromEnum t

instance Bounded v => Bounded (Cell v) where

  minBound = Cell minBound minBound

  maxBound = Cell maxBound maxBound

-- | List all potential attackers for a given square:
--   x attacks y iff (x, value at x) `elem` attackers board y
attackers :: Num v => Board -> Coord -> [(Coord, Cell v)]
attackers b@(Board n) (Coord r c) =
  map (\(c, v) -> (c, Cell (fromIntegral v) False)) $ filter (\(c, v) -> inBoard b c) $
    [(Coord (r - i) c, i) | i <- [1 .. n]]
      ++ [(Coord (r + i) c, i) | i <- [1 .. n]]
      ++ [(Coord r (c - i), i) | i <- [1 .. n]]
      ++ [(Coord r (c + i), i) | i <- [1 .. n]]

{-
  Encoding the board for Holmes
-}

definedConfig :: Board -> Config Holmes (Defined (Cell Int))
definedConfig (Board n) = (n * n) `from` [Cell v c | v <- [1 .. n], c <- [True, False]]

-- | Project from Cell to the number in the cell
toNum :: (Mapping f c, c (Cell v), c v) => Prop m (f (Cell v)) -> Prop m (f v)
toNum p = value .$ p

-- | Project from Cell to the isTower flag
toTower :: (Mapping f c, c (Cell v), c Bool) => Prop m (f (Cell v)) -> Prop m (f Bool)
toTower p = tower .$ p

{-
  Puzzle rules as constraints
-}

-- | Prescribe the number value at the given index.
givenNum ::
  ( Mapping f c,
    c (Cell v),
    c v,
    EqR (f v) (f Bool),
    MonadCell m,
    Applicative f
  ) =>
  Int ->
  v ->
  [Prop m (f (Cell v))] ->
  Prop m (f Bool)
givenNum i v cells = toNum (cells !! i) .== lift (pure v)

-- | There is a tower at the given index.
isTower ::
  (Mapping f c, c (Cell v), c Bool) =>
  Int ->
  [Prop m (f (Cell v))] ->
  Prop m (f Bool)
isTower i cells = toTower (cells !! i)

-- given a list of values at indexes, count how many of these occur
countEqual ::
  (Eq x, Mapping f c, c x, c v, Num v, Num (f v), SumR (f v), MonadCell m) =>
  [(Int, x)] ->
  [Prop m (f x)] ->
  Prop m (f v)
countEqual vals cells = foldl (.+) (lift 0) (map f vals)
  where
    isEqual v w = if w == v then 1 else 0
    f (i, v) = isEqual v .$ (cells !! i)

towerConstraint ::
  ( Mapping f c,
    c (Cell v),
    c v,
    Eq v,
    OrdR (f v) (f Bool),
    Num v,
    Num (f v),
    SumR (f v),
    MonadCell m
  ) =>
  Board ->
  Coord ->
  [Prop m (f (Cell v))] ->
  Prop m (f Bool)
towerConstraint board coord cells = eq .>= toNum (cells !! toIndex board coord)
  where
    as = attackers board coord
    eq = countEqual [(toIndex board c, v) | (c, v) <- as] cells

latinSquare' ::
  (Mapping f c, c (Cell v), c v, MonadCell m, EqR (f v) (f Bool)) =>
  Board ->
  [Prop m (f (Cell v))] ->
  Prop m (f Bool)
latinSquare' board = latinSquare board . map toNum

latinSquare ::
  (MonadCell m, EqR x b) => Board -> [Prop m x] -> Prop m b
latinSquare board cells =
  and'
    [ all' distinct (cols board cells),
      all' distinct (rows board cells)
    ]

{-
  Interacting with the solver
-}

showSolution :: Board -> [Defined (Cell Int)] -> IO ()
showSolution board sol = case extractSol of
  Nothing -> putStrLn "invalid solution"
  Just s -> showSol s
  where
    extractSol = if length exacts == length sol then Just exacts else Nothing
    exacts = mapMaybe fromDefined sol
    fromDefined (Exactly x) = Just x
    fromDefined _ = Nothing
    showSol ss = mapM_ showRow $ rows board ss
    showRow r = putStrLn $ concat $ map (show . value) $ r

solve ::
  Board ->
  Config Holmes (Defined (Cell Int)) ->
  (forall m. MonadCell m => [Prop m (Defined (Cell Int))] -> Prop m (Defined Bool)) ->
  IO ()
solve board config constraints = do
  s <- config `satisfying` constraints
  case s of
    Nothing -> putStrLn "no solution"
    Just sol -> showSolution board sol

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
{-
config6 :: Config Holmes (Defined Cell)
config6 = definedConfig (Board 6)

puzzle6 :: MonadCell m => [Prop m (Defined Cell)] -> Prop m (Defined Bool)
puzzle6 board =
   and'
        [ and' (map (\pos -> isTower pos board) towers)
        , and' [ g 0 (6, False)
               , g 1 (3, True)
               , g 2 (2, True)
               , g 3 (1, True)
               , g 4 (5, False)
               , g 5 (4, False)
               , g 23 (2, True)
               , g 29 (3, True)
               , g 35 (1, True)
               , g 13 (2, False) ]
        --, and' (map (\pos -> not' $ isTower pos board) nonTowers)
        , latinSquare' board
        , and' (map towerConstraint towers)
        ]
  where
    --g :: MonadCell m => Position -> Cell -> [ Prop m (Defined Cell) ] -> Prop m (Defined Bool)
    g pos x = given pos (pure x) board
    towers = [1,2,3,23,29,35]
    --nonTowers = filter (not . flip elem towers) [0..35]
    towerConstraint pos = countEqual (nonTowerAttackers $ attackers pos) board .>= toNum (board !! pos)

-}
