module Interval where

data Interval = Interval {
    bMin :: !Double,
    bMax :: !Double
}

infinity :: Double
infinity = 1 / 0

defaultInterval :: Interval
defaultInterval = Interval (-infinity) infinity

contains :: Double -> Interval -> Bool
contains val (Interval minB maxB)  = minB <= val && val <= maxB

size :: Interval -> Double
size (Interval minB maxB) = maxB - minB

surrounds :: Double -> Interval -> Bool
surrounds val (Interval minB maxB) = minB < val && val < maxB

clamp :: Double -> Interval -> Double
clamp x (Interval minB maxB)
    | x < minB = minB
    | x > maxB = maxB
    | otherwise = x

empty :: Interval
empty = Interval infinity (-infinity)

universe :: Interval
universe = Interval (-infinity) infinity