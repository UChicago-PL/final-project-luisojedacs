module Sphere where
import Hittable
import Vec3
import Ray (Ray(..), at)

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
                - sqrt (discriminantCRadR c rad r)) 
                / aVar r

--inclusive
inBounds :: Double -> Double -> Double -> Bool
inBounds val bMin bMax = bMin <= val && val <= bMax

validRoot :: Sphere -> Ray -> Double -> Double -> Maybe Double
validRoot (Sphere c rad) r tMin tMax
    | inBounds (minusRoot c rad r) tMin tMax = Just $ minusRoot c rad r
    | inBounds (plusRoot c rad r) tMin tMax = Just $ plusRoot c rad r
    | otherwise = Nothing

normalVec :: Point3 -> Point3 -> Double -> Vec3
normalVec p3 c = scaleDown (p3 - c)

instance Hittable Sphere where
    hit (Sphere c rad) r tMin tMax = 
        if discriminantCRadR c rad r
            < 0 
            then Nothing
        else fmap 
                (\root -> 
                    hrFaceNormal (at r root) (normalVec (at r root) c rad) root
                )
                (validRoot (Sphere c rad) r tMin tMax)