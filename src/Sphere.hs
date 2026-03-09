module Sphere where
import Hittable
import Vec3
import Ray (Ray(..), at)
import Interval (contains, Interval)

--Sphere hit logic
--Derivation is in the book Ray Tracing in One Weekend
-- quadratic formula discriminant
discriminant :: Double -> Double -> Double -> Double
discriminant a h c = h * h - a * c

-- quadratic formula variables
aVar :: Ray -> Double
aVar r = vecLengthSquared (direction r)

hVar :: Ray -> Point3 -> Double
hVar r c = dotProduct (direction r) (oToC r c)

cVar :: Vec3 -> Double -> Double
cVar oc rad = vecLengthSquared oc - (rad * rad)
-- ray-origin-to-center-of-sphere vector
oToC :: Ray -> Point3 -> Vec3
oToC r c = c - origin r

discriminantCRadR :: Point3 -> Double -> Ray -> Double
discriminantCRadR c rad r = discriminant (aVar r) 
                                 (hVar r c)
                                 (cVar (oToC r c) rad)

data Sphere = Sphere {
    center :: Point3,
    radius :: Double
} deriving (Eq)

makeSphere :: Point3 -> Double -> Sphere
makeSphere c rad = Sphere c (max rad 0)

minusRoot :: Point3 -> Double -> Ray -> Double
minusRoot c rad r = (hVar r c
                - sqrt (discriminantCRadR c rad r)) 
                / aVar r

plusRoot :: Point3 -> Double -> Ray -> Double
plusRoot c rad r = (hVar r c
                + sqrt (discriminantCRadR c rad r)) 
                / aVar r

validRoot :: Sphere -> Ray -> Interval -> Maybe Double
validRoot (Sphere c rad) r intval
    | contains (minusRoot c rad r) intval = Just $ minusRoot c rad r
    | contains (plusRoot c rad r) intval= Just $ plusRoot c rad r
    | otherwise = Nothing

normalVec :: Point3 -> Point3 -> Double -> Vec3
normalVec p3 c = scaleDown (p3 - c)

instance Hittable Sphere where
    hit (Sphere c rad) r intval = 
        if discriminantCRadR c rad r
            < 0 
            then Nothing
        else fmap
                (\root -> 
                    hrFaceNormal (at r root) root r (normalVec (at r root) c rad)
                )
                (validRoot (Sphere c rad) r intval)