module Hittable where
import Ray (Ray(..))
import Vec3 (Vec3(..), Point3, dotProduct)

data HitRecord = HitRecord {
    p :: Point3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool
} deriving (Eq)

hrFaceNormal :: Point3 -> Vec3 -> Double -> Bool -> Ray -> Vec3 -> HitRecord
hrFaceNormal pt _ dist _ r outwardNormal
    | fF = HitRecord pt outwardNormal dist fF
    | otherwise = HitRecord pt (-outwardNormal) dist fF
        where
            fF = dotProduct (direction r) outwardNormal < 0

class Hittable a where
    --In the book, the hit fn returns a Bool because the HitRecord object is passed by ref and
    -- is mutated in place. Haskell isn't stateful in this way, so we simply return Maybe HitRecord
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord
