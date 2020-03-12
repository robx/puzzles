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

data Val4 = V4 Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

instance Num Val4 where
  fromInteger = toEnum . pred . fromInteger

instance Enum Val4 where

  toEnum n
    | n < 0 || n >= 4 = error "Val4 out of bounds"
    | otherwise = V4 (n + 1)

  fromEnum (V4 m) = m - 1

instance Bounded Val4 where

  minBound = toEnum 0

  maxBound = toEnum 3

{-
  Encoding the board for Holmes
-}

intersectConfig :: Board -> Config Holmes (Intersect (Cell Val4))
intersectConfig (Board n) =
  case n of
    4 -> (n * n) `from` [Cell v t | v <- [1 .. 4], t <- [False, True]]
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
