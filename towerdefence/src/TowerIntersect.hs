{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module TowerIntersect where

import Data.Hashable (Hashable)
import Data.Holmes
import Data.JoinSemilattice.Intersect (toList)
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import Data.Propagator
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Tower hiding (showSolution)

-- | Value type for 4x4 tower defence puzzle.
--
-- We allow 0..4 so that we can compare with 0
-- as an attacker count.
newtype Val4 = V4 Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Num Val4 where

  fromInteger = toEnum . fromInteger

  -- this is not a valid Num instance, we just want to use
  -- it for counting
  (V4 a) + (V4 b) = if a + b > 4 then V4 4 else V4 (a + b)

instance Enum Val4 where

  toEnum n
    | n < 0 || n > 4 = error $ "toEnum Val4 out of bounds: " ++ show n
    | otherwise = V4 n

  fromEnum v@(V4 m)
    | m < 0 || m > 4 = error $ "fromEnum Val4 out of bounds: " ++ show v
    | otherwise = m

instance Bounded Val4 where

  minBound = toEnum 0

  maxBound = toEnum 4

-- given a list of values at indexes, count how many of these occur
countEqualMono ::
  MonadCell m =>
  [(Int, Val4)] ->
  [Prop m (Intersect Val4)] ->
  Prop m (Intersect Val4)
countEqualMono vals cells = foldr (.+) (lift 0) (map f vals)
  where
    isEqual v w = if w == v then 1 else 0
    f (i, v) = isEqual v .$ (cells !! i)

{-
  Encoding the board for Holmes
-}

intersectConfig :: Board -> Config Holmes (Intersect (Cell Val4))
intersectConfig (Board n) =
  case n of
    4 -> (n * n) `from` [Cell v t | v <- [1 .. 4], t <- [False, True]]
    -- 1..3 fits in Val4...
    3 -> (n * n) `from` [Cell v t | v <- [1 .. 3], t <- [False, True]]
    2 -> (n * n) `from` [Cell v t | v <- [1 .. 2], t <- [False, True]]
    1 -> (n * n) `from` [Cell v t | v <- [1 .. 1], t <- [False, True]]
    _ -> error "board size not implemented"

{-
  Interacting with the solver
-}

showSolution ::
  (Show v, Bounded v, Enum v, Eq v) =>
  Board ->
  [Intersect (Cell v)] ->
  IO ()
showSolution board sol = case extractSol of
  Nothing -> putStrLn "invalid solution"
  Just s -> showSol s
  where
    extractSol = if length exacts == length sol then Just exacts else Nothing
    exacts = mapMaybe (fromDefined . toList) sol
    fromDefined [x] = Just x
    fromDefined _ = Nothing
    showSol ss = mapM_ showRow $ rows board ss
    showRow r = putStrLn $ concat $ map (show . value) $ r

solve ::
  (Bounded v, Enum v, Ord v, Hashable v, Show v, Typeable v) =>
  Board ->
  Config Holmes (Intersect (Cell v)) ->
  (forall m. MonadCell m => [Prop m (Intersect (Cell v))] -> Prop m (Intersect Bool)) ->
  IO ()
solve board config constraints = do
  s <- config `satisfying` constraints
  case s of
    Nothing -> putStrLn "no solution"
    Just sol -> showSolution board sol
