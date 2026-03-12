module Vec3 where
-- Module for the Vec3 datatype --

-- Define the Vec3 datatype
data Vec3 = Vec3 {
    e0 :: !Double,
    e1 :: !Double,
    e2 :: !Double
} deriving (Eq, Read)

-- In case this becomes useful for PPM files
instance Show Vec3 where
    show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z

-- Haskell is highly convenient in this regard--strong typing system etc. means
-- that we have a natural way of adding the properties we want through the typeclass
-- system:
-- Make Vec3 an instance of Num
instance Num Vec3 where
    negate (Vec3 x0 x1 x2) = Vec3 (-x0) (-x1) (-x2)
    (+) (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x0 + y0) (x1 + y1) (x2 + y2)
    (*) (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x0 * y0) (x1 * y1) (x2 * y2) --following the raytracing book here
    abs (Vec3 x0 x1 x2) = Vec3 (abs x0) (abs x1) (abs x2)
    signum (Vec3 x0 x1 x2) = Vec3 (signum x0) (signum x1) (signum x2)
    fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i) -- just make it a vec with the same one

-- Function for vector refraction (unit vector, normal vector of surface, eta_i over eta_t), 
-- returns refracted vector
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etaiOverEtat = rOutPerp + rOutPar
    where
        cosTheta = min (dotProduct (-uv) n) 1
        rOutPerp = scale (uv + scale n cosTheta) etaiOverEtat
        rOutPar = scale n (-(sqrt (1 - vecLengthSquared rOutPerp)))

-- Function for Vec3 reflection (Vec3, normal Vec3 of surface) , returns reflected Vec3
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - scale n (2 * dotProduct v n)

-- Determine if a Vec3 is near zero in all of its entries
nearZero :: Vec3 -> Bool
nearZero (Vec3 x0 x1 x2) = abs x0 < 1e-8 && abs x1 < 1e-8 && abs x2 < 1e-8

-- Apply a function (double -> double) to each entry of the Vec3
mapVec :: (Double -> Double) -> Vec3 -> Vec3
mapVec f (Vec3 x0 x1 x2) = Vec3 (f x0) (f x1) (f x2)

-- Scale a Vec3
{-# INLINE scale #-}
scale :: Vec3 -> Double -> Vec3
scale v t = mapVec (*t) v

-- Scale down a Vec3
scaleDown :: Vec3 -> Double -> Vec3
scaleDown v t = scale v (recip t)

-- Dot product of two Vec3s
{-# INLINE dotProduct #-}
dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = (x0 * y0) + (x1 * y1) + (x2 * y2)

-- Cross product of two Vec3s
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x1 * y2 - x2 * y1)
                                             (x2 * y0 - x0 * y2)
                                             (x0 * y1 - x1 * y0)

-- Unit vector for a Vec3
{-# INLINE unitVector #-}
unitVector :: Vec3 -> Vec3
unitVector v = scaleDown v (vecLength v)

-- Squared length of a Vec3
{-# INLINE vecLengthSquared #-}
vecLengthSquared :: Vec3 -> Double
vecLengthSquared (Vec3 x y z) = (x * x) + (y * y) + (z * z)

-- Length of a Vec3
vecLength :: Vec3 -> Double
vecLength vec = sqrt $ vecLengthSquared vec

-- Access a Vec3 entry
(!!) :: Vec3 -> Int -> Double
(!!) (Vec3 x _ _) 0 = x
(!!) (Vec3 _ y _) 1 = y
(!!) (Vec3 _ _ z) 2 = z
(!!) _ _ = error "Invalid index for Vec3"

-- Alias for Vec3 (Point3)
type Point3 = Vec3