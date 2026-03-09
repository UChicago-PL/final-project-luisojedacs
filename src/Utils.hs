module Utils where
import Vec3 (Vec3(..), unitVector, Point3, vecLengthSquared, dotProduct)
import Ray (Ray(..), at)
import Interval (infinity, Interval (..))
import System.Random ( Random(randomR), StdGen )

-- use stdgen to take from std distro
-- will need to pass in distro in main using getStdGen
-- randomR to find random value in closed interval
-- note--don't need doubleDegreesToRadians bc we already have Floating as the constraint in our impl
randomDouble :: StdGen -> (Double, StdGen)
randomDouble = randomR (0.0, 1.0)

randomDoubleIntval :: Interval -> StdGen -> (Double, StdGen)
randomDoubleIntval (Interval minB maxB) g = (minB + (maxB - minB) * fst (randomDouble g), snd (randomDouble g))

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
