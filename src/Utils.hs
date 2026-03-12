{-# LANGUAGE ExistentialQuantification #-}
module Utils where
-- Module where many utils (and miscellaneous classes are defined) --

import Vec3 (Vec3(..), unitVector, Point3, vecLengthSquared, dotProduct, scaleDown)
import Ray (Ray(..))
import Interval (Interval (..))
import System.Random ( Random(randomR), StdGen )
import Color (Color)

-- Definition of HitRecord datatype
data HitRecord = HitRecord { -- strict fields
    p :: !Point3,
    normal :: !Vec3,
    t :: !Double,
    frontFace :: !Bool,
    material :: !SomeMaterial
}

-- Defining an existential datatype to act as a wrapper for a particular material
data SomeMaterial = forall a. Material a => SomeMaterial a

-- Defining the Material typeclass
class Material a where
    scatter :: StdGen -> Ray -> HitRecord -> a -> Maybe (Color, Ray, StdGen)

-- Use stdgen to take from std distro
-- randomR is used to find a random value in closed interval
randomDouble :: StdGen -> (Double, StdGen)
randomDouble = randomR (0.0, 1.0)

-- Like randomDouble, but constrained to an interval
randomDoubleIntval :: StdGen -> Interval -> (Double, StdGen)
randomDoubleIntval g (Interval minB maxB) = (minB + (maxB - minB) * val, g')
      where
            (val, g') = randomDouble g

-- Generates a Vec3 of random doubles
randomVec :: StdGen -> (Vec3, StdGen)
randomVec g = (Vec3 x y z, g3)
    where
        (x, g1) = randomDouble g
        (y, g2) = randomDouble g1
        (z, g3) = randomDouble g2

-- Generates a Vec3 of random doubles within a particular interval
randomVecIntval :: StdGen -> Interval -> (Vec3, StdGen)
randomVecIntval g i = (Vec3 x y z, g3)
    where
        (x, g1) = randomDoubleIntval g i
        (y, g2) = randomDoubleIntval g1 i
        (z, g3) = randomDoubleIntval g2 i

-- Generates a unit vector of random doubles
randomUnitVec :: StdGen -> (Vec3, StdGen)
randomUnitVec g
      --prevent a blow-up with sqrt 1e-160 in the divisor
      | lenSq <= 1 && lenSq > 1e-160 = (scaleDown pt (sqrt lenSq), g')
      | otherwise = randomUnitVec g'
      where
            (pt, g') = randomVecIntval g (Interval (-1) 1)
            lenSq = vecLengthSquared pt

-- Generates a random Vec3 on a hemisphere using the rejection method--generate until you find
-- a Vec3 that is facing the same direction as a normal vector (the normal of the hemisphere facing
-- the camera)
randomOnHemisphere :: StdGen -> Vec3 -> (Vec3, StdGen)
randomOnHemisphere g n
      | dotProduct onUnit n > 0 = (onUnit, g')
      | otherwise = (-onUnit, g')
      where
            (onUnit, g') = randomUnitVec g

-- Generates a unit vector pointing in the direction of a ray
unitDirection :: Ray -> Vec3
unitDirection = unitVector . direction

alpha :: Ray -> Double --for scaling blue/white based on y coord topdown--linear blend below
alpha r = 0.5 * (e1 (unitDirection r) + 1.0)

-- Converts degrees to radians
degreesToRadians :: Floating a => a -> a
degreesToRadians degrees = degrees * pi / 180
