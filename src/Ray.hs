module Ray where
import Vec3 (Point3, Vec3(..), scale)
-- Module for the Ray datatype --

data Ray = Ray { -- strict fields
    origin :: !Point3,
    direction :: !Vec3
} deriving (Eq)

-- Return the point at a certain length along the ray
at :: Ray -> Double -> Point3
at (Ray orig dir) t = orig + scale dir t
