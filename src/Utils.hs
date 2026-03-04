module Utils where
import Vec3 (Vec3(..), unitVector, scale, Point3, vecLengthSquared, dotProduct)
import Color (Color)
import Ray (Ray(..))

--Sphere hit logic
--Derivation is in the book Ray Tracing in One Weekend
-- quadratic formula discriminant
discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b * b - 4 * a * c

-- quadratic formula variables
aVar :: Ray -> Double
aVar r = vecLengthSquared (direction r)

bVar :: Ray -> Vec3 -> Double
bVar r oc = (-2.0) * dotProduct (direction r) oc

cVar :: Vec3 -> Double -> Double
cVar oc rad = vecLengthSquared oc - (rad * rad)
-- ray-origin-to-center-of-sphere vector
oToC :: Ray -> Point3 -> Vec3
oToC r center = center - origin r

hitSphere :: Point3 -> Double -> Ray -> Bool -- return true if discriminant >= 0
hitSphere center rad r =    discriminant   (aVar r) 
                                        (bVar r (oToC r center)) 
                                        (cVar (oToC r center) rad)
                            >= 0

mapVec :: (Double -> Double) -> Vec3 -> Vec3
mapVec f (Vec3 x0 x1 x2) = Vec3 (f x0) (f x1) (f x2)

unitDirection :: Ray -> Vec3
unitDirection = unitVector . direction

alpha :: Ray -> Double --for scaling blue/white based on y coord topdown--linear blend below
alpha r = 0.5 * (e1 (unitDirection r) + 1.0)

rayColor :: Ray -> Color
rayColor r 
    | hitSphere (Vec3 0 0 (-1)) 0.5 r = Vec3 1 0 0
    | otherwise = scale (Vec3 1.0 1.0 1.0) (1 - alpha r) + scale (Vec3 0.5 0.7 1.0) (alpha r)