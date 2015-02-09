module Interval where

import Data.SBV

type Ix = Int
type Edge = (Ix, Ix)

data Vars = Vars {
      val :: Ix -> SBool
    , fwd :: Ix -> SBool
    , bwd :: Ix -> SBool
}

vars :: Int -> Symbolic Vars
vars n = do
    vs <- mkExistVars n
    fs <- mkExistVars (n - 1)
    bs <- mkExistVars (n - 1)
    return (Vars (vs !!) (fs !!) ((bs !!) . (+ (-1))))

type Prob = (Int, Int, Int)

--prob :: Prob -> Vars -> SBool
--prob (n, k, s) vs =
--    val vs k &&& (size vars k .== fromIntegral s)

count :: (a -> SBool) -> [a] -> Symbolic (SBool, SWord8)
count p xs = do
    ys <- mkExistVars (length xs)
    let f x y = (p x <=> (y .== 1)) &&& (bnot (p x) <=> (y .== 0))
    let prop = bAll (uncurry f) (zip xs ys)
    return (prop, sum ys)

countEq :: (a -> SBool) -> [a] -> SWord8 -> Symbolic SBool
countEq p xs v = do
    (prop, c) <- count p xs
    return $ prop &&& c .== v
