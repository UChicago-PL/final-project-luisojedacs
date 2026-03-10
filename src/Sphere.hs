{-# LANGUAGE InstanceSigs #-}
module Sphere where
import Hittable
import Vec3
import Ray (Ray(..), at)
import Interval (contains, Interval)
import Utils (SomeMaterial, HitRecord)

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
    center :: !Point3,
    radius :: !Double,
    material :: !SomeMaterial
}

makeSphere :: Point3 -> Double -> SomeMaterial -> Sphere
makeSphere c rad = Sphere c (max rad 0) 

validRoot :: Sphere -> Ray -> Interval -> Maybe Double
validRoot (Sphere c rad _) r intval
    | contains minusR intval = Just $ minusR
    | contains plusR intval= Just $ plusR
    | otherwise = Nothing
    where
        h = hVar r c
        disc = discriminantCRadR c rad r
        a = aVar r
        plusR = (h + sqrt disc) / a
        minusR = (h - sqrt disc) / a
        



normalVec :: Point3 -> Point3 -> Double -> Vec3
normalVec p3 c = scaleDown (p3 - c)

instance Hittable Sphere where
    hit :: Sphere -> Ray -> Interval -> Maybe HitRecord
    hit (Sphere c rad mat) r intval = 
        if discriminantCRadR c rad r
            < 0 
            then Nothing
        else fmap
                (\root -> 
                    hrFaceNormal (at r root) root r (normalVec (at r root) c rad) mat
                )
                (validRoot (Sphere c rad mat) r intval)