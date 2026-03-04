module Ray where
import Vec3 (Point3, Vec3(..), scale)

--Module for the Ray class
data Ray = Ray {
    origin :: Point3,
    direction :: Vec3
} deriving (Eq)

at :: Ray -> Double -> Point3
at (Ray orig dir) t = orig + scale dir t
