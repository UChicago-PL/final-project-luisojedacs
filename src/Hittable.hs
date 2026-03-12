module Hittable where
-- The module defining the Hittable class --
import Ray (Ray(..))
import Vec3 (Vec3(..), Point3, dotProduct)
import Interval (Interval)
import Utils (HitRecord(..), SomeMaterial)

--Return a hitrecord with the outwardNormal vector set to face away from the surface
hrFaceNormal :: Point3 -> Double -> Ray -> Vec3 -> SomeMaterial -> HitRecord
hrFaceNormal pt dist r outwardNormal mat
    | fF = HitRecord pt outwardNormal dist fF mat
    | otherwise = HitRecord pt (-outwardNormal) dist fF mat
        where
            fF = dotProduct (direction r) outwardNormal < 0

class Hittable a where
    -- In the book, the hit fn returns a Bool because the HitRecord object is passed by ref and
    -- is mutated in place. Haskell isn't stateful in this way, so we return Maybe HitRecord instead
    -- This pattern reoccurs throughout the project
    -- A complete instance of Hittable simply implements hit
    hit :: a -> Ray -> Interval -> Maybe HitRecord
