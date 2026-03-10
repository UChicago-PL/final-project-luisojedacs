{-# LANGUAGE ExistentialQuantification #-}
module Utils where
import Vec3 (Vec3(..), unitVector, Point3, vecLengthSquared, dotProduct, scaleDown)
import Ray (Ray(..), at)
import Interval (infinity, Interval (..))
import System.Random ( Random(randomR), StdGen )
import Color (Color)

data HitRecord = HitRecord {
    p :: Point3,
    normal :: Vec3,
    t :: Double,
    frontFace :: Bool,
    material :: SomeMaterial
}

data SomeMaterial = forall a. Material a => SomeMaterial a

class Material a where
    scatter :: StdGen -> Ray -> HitRecord -> a -> Maybe (Color, Ray, StdGen)

-- use stdgen to take from std distro
-- will need to pass in distro in main using getStdGen
-- randomR to find random value in closed interval
-- note--don't need doubleDegreesToRadians bc we already have Floating as the constraint in our impl
randomDouble :: StdGen -> (Double, StdGen)
randomDouble = randomR (0.0, 1.0)

randomDoubleIntval :: StdGen -> Interval -> (Double, StdGen)
randomDoubleIntval g (Interval minB maxB) = (minB + (maxB - minB) * val, g')
      where
            (val, g') = randomDouble g

--TODO--figure out how to get these in vec
randomVec :: StdGen -> (Vec3, StdGen)
randomVec g = (Vec3 x y z, g3)
    where
        (x, g1) = randomDouble g
        (y, g2) = randomDouble g1
        (z, g3) = randomDouble g2

randomVecIntval :: StdGen -> Interval -> (Vec3, StdGen)
randomVecIntval g i = (Vec3 x y z, g3)
    where
        (x, g1) = randomDoubleIntval g i
        (y, g2) = randomDoubleIntval g1 i
        (z, g3) = randomDoubleIntval g2 i

randomUnitVec :: StdGen -> (Vec3, StdGen)
randomUnitVec g
      --prevent a blow-up with sqrt 1e-160 in the divisor
      | lenSq <= 1 && lenSq > 1e-160 = (scaleDown p (sqrt lenSq), g')
      --if conditions not met, 
      | otherwise = randomUnitVec g'
      where
            (p, g') = randomVecIntval g (Interval (-1) 1)
            lenSq = vecLengthSquared p

randomOnHemisphere :: StdGen -> Vec3 -> (Vec3, StdGen)
randomOnHemisphere g normal
      | dotProduct onUnit normal > 0 = (onUnit, g')
      | otherwise = (-onUnit, g')
      where
            (onUnit, g') = randomUnitVec g

--Sphere hit logic
--Derivation is in the book Ray Tracing in One Weekend
-- quadratic formula discriminant
discriminant :: Double -> Double -> Double -> Double
discriminant a h c = h * h - a * c

-- quadratic formula variables
aVar :: Ray -> Double
aVar r = vecLengthSquared (direction r)

hVar :: Ray -> Point3 -> Double
hVar r center = dotProduct (direction r) (oToC r center)

cVar :: Vec3 -> Double -> Double
cVar oc rad = vecLengthSquared oc - (rad * rad)
-- ray-origin-to-center-of-sphere vector
oToC :: Ray -> Point3 -> Vec3
oToC r center = center - origin r

discriminantCRadR :: Point3 -> Double -> Ray -> Double
discriminantCRadR center rad r = discriminant (aVar r) 
                                 (hVar r center)
                                 (cVar (oToC r center) rad)

hitSphere :: Point3 -> Double -> Ray -> Double
hitSphere center rad r =    if discriminantCRadR center rad r
                                < 0 then -1
                            else (hVar r center
                                  - sqrt (discriminantCRadR center rad r)) 
                                  / aVar r

unitDirection :: Ray -> Vec3
unitDirection = unitVector . direction

alpha :: Ray -> Double --for scaling blue/white based on y coord topdown--linear blend below
alpha r = 0.5 * (e1 (unitDirection r) + 1.0)

shadeSphere :: Ray -> Double -> Point3
shadeSphere r t = unitVector $ at r t - Vec3 0 0 (-1)

degreesToRadians :: Floating a => a -> a
degreesToRadians degrees = degrees * pi / 180
