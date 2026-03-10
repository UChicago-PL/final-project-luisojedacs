module Vec3 where -- change this later

--Module for the Vec3 class
data Vec3 = Vec3 { -- maybe change fields TODO
    e0 :: Double,
    e1 :: Double,
    e2 :: Double
} deriving (Eq, Read) -- Maybe not ord--we will see

instance Show Vec3 where -- may be useful for ppm
    show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z

-- should have e0, e1, e2 as fns out of the box--using records for convenience

-- Haskell is highly convenient in this regard--strong typing system etc. means
-- that we have a natural way of adding the properties we want through the typeclass
-- system:
instance Num Vec3 where
    negate (Vec3 x0 x1 x2) = Vec3 (-x0) (-x1) (-x2)
    (+) (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x0 + y0) (x1 + y1) (x2 + y2)
    (*) (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x0 * y0) (x1 * y1) (x2 * y2) --following the raytracing book here
    abs (Vec3 x0 x1 x2) = Vec3 (abs x0) (abs x1) (abs x2)
    signum (Vec3 x0 x1 x2) = Vec3 (signum x0) (signum x1) (signum x2)
    fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i) -- just make it a vec with the same one

mapVec :: (Double -> Double) -> Vec3 -> Vec3
mapVec f (Vec3 x0 x1 x2) = Vec3 (f x0) (f x1) (f x2)

scale :: Vec3 -> Double -> Vec3
scale v t = mapVec (*t) v

scaleDown :: Vec3 -> Double -> Vec3
scaleDown v t = scale v (recip t)

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = (x0 * y0) + (x1 * y1) + (x2 * y2)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x0 x1 x2) (Vec3 y0 y1 y2) = Vec3 (x1 * y2 - x2 * y1)
                                             (x2 * y0 - x0 * y2)
                                             (x0 * y1 - x1 * y0)

unitVector :: Vec3 -> Vec3
unitVector v = scaleDown v (vecLength v)

vecLengthSquared :: Vec3 -> Double
vecLengthSquared (Vec3 x y z) = (x * x) + (y * y) + (z * z)

vecLength :: Vec3 -> Double
vecLength vec = sqrt $ vecLengthSquared vec

(!!) :: Vec3 -> Int -> Double
(!!) (Vec3 x _ _) 0 = x
(!!) (Vec3 _ y _) 1 = y
(!!) (Vec3 _ _ z) 2 = z
(!!) _ _ = error "Invalid index for Vec3"

type Point3 = Vec3