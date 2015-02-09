module Logic where

import Prelude hiding (sequence)

import Control.Monad hiding (sequence)
import Data.Traversable (sequence)

import qualified Data.Map as Map
import qualified Data.Set as Set

type Coord = (Int, Int)
type Coords = Set.Set Coord
type Size = (Int, Int)
type Puzzle = (Size, [(Coord, Int)])

data Val = Shaded | Unshaded
    deriving (Show, Eq)

type Grid = Map.Map Coord Val

vals :: MonadPlus m => m Val
vals = return Shaded `mplus` return Unshaded

coords :: Size -> [Coord]
coords (mx,my) = [(x,y) | x <- [1..mx], y <- [1..my]]

grid :: MonadPlus m => Size -> m Grid
grid sz = sequence $ Map.fromList (zip (coords sz) (repeat vals))

component :: Coord -> Grid -> Coords
component c g = execState (comp c) g
  where
    comp c = do
        v <- case 
    | Map.lookup g c == Shaded  = Set.empty
    | otherwise                 = Set.singleton c `Set.union`
solve :: MonadPlus m => Puzzle -> m Grid
solve (sz, clues) = do
    g <- grid sz

-- | Colour a graph.
colourM :: (Ord k, Eq a) => (k -> [k]) -> Map.Map k a -> Map.Map k Int
colourM nbrs m = fmap fromRight . execState colour' $ start
  where
    fromRight (Right r) = r
    fromRight (Left _)  = error "expected Right"

    start = fmap (const $ Left [1..]) m
    colour' = mapM_ pickAndFill (Map.keys m)

    -- choose a colour for the given node, and spread it to
    -- equal neighbours, removing it from unequal neighbours
    pickAndFill x = do
        v <- (Map.! x) <$> get
        case v of
            Left (c:_) -> fill (m Map.! x) c x
            Left _     -> error "empty set of candidates"
            Right _    -> return ()

    fill a c x = do
        v <- (Map.! x) <$> get
        case v of
            Left _     -> if m Map.! x == a
                    then do modify (Map.insert x (Right c))
                            mapM_ (fill a c) (nbrs x)
                    else modify (del x c)
            Right _    -> return ()

    -- remove the given colour from the list of candidates
    del x c = Map.adjust f x
      where
        f (Left cs) = Left $ filter (/= c) cs
        f (Right c') = Right c'

