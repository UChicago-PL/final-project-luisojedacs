module Interval where

-- The class defining the interval datatype, helper functions, et cetera --

data Interval = Interval {
    bMin :: !Double,
    bMax :: !Double
}

-- Define infinity
infinity :: Double
infinity = 1 / 0

-- The default interval
defaultInterval :: Interval
defaultInterval = Interval (-infinity) infinity

-- Return whether an interval contains (inclusive) a double
contains :: Double -> Interval -> Bool
contains val (Interval minB maxB)  = minB <= val && val <= maxB

-- Return the size of the interval
size :: Interval -> Double
size (Interval minB maxB) = maxB - minB

-- Check if an interval surrounds (exclusive) a double
surrounds :: Double -> Interval -> Bool
surrounds val (Interval minB maxB) = minB < val && val < maxB

-- If out of bounds of the interval, choose whichever bound is closer
clamp :: Double -> Interval -> Double
clamp x (Interval minB maxB)
    | x < minB = minB
    | x > maxB = maxB
    | otherwise = x

-- An empty interval
empty :: Interval
empty = Interval infinity (-infinity)

-- An infinite interval
universe :: Interval
universe = Interval (-infinity) infinity