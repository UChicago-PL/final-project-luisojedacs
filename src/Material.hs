{-# LANGUAGE ExistentialQuantification #-} -- for existential wrapper (instead of shared ptr)
module Material where
-- File where Material types are instantiated --

import Ray (Ray (..))
import Color (Color)
import Vec3 (Vec3 (..), nearZero, reflect, unitVector, scale, dotProduct, refract)
import Utils (randomUnitVec, Material (scatter), HitRecord (..), randomDouble)

-- Define Dielectric as a newtype
newtype Dielectric = Dielectric {
    refractionIndex :: Double
}

-- Shlick Approximation for reflectance
reflectance :: Double -> Double -> Double
reflectance cosine refrIdx = r0 + (1 - r0) * ((1 - cosine) ** 5)
    where
        r0 = ((1 - refrIdx) / (1 + refrIdx)) ** 2

-- Define dielectric's scatter function
instance Material Dielectric where
    scatter g rIn hr diel = Just (attenuation, scattered, g1)
        where
            ri = if frontFace hr then recip (refractionIndex diel) else refractionIndex diel
            attenuation = Vec3 1 1 1
            unitDirection = unitVector (direction rIn)
            cosTheta = min (dotProduct (-unitDirection) (normal hr)) 1
            sinTheta = sqrt (1 - (cosTheta * cosTheta))
            (randDoub, g1) = randomDouble g
            cannotRefract = ri * sinTheta > 1
            dir =   if cannotRefract || reflectance cosTheta ri > randDoub
                    then reflect unitDirection (normal hr)
                    else refract unitDirection (normal hr) ri
            scattered = Ray (p hr) dir

-- Define Metal as data
data Metal = Metal {
    albedoM :: Color,
    fuzz :: Double
}

-- Retrieve the fuzz factor for the reflection given a particular metal
getFuzz :: Metal -> Double
getFuzz m
    | fuzz m < 1 = fuzz m
    | otherwise = 1

-- Define metal's scatter function
instance Material Metal where
    scatter g rIn hr metal
        | dotProduct (direction scattered) (normal hr) > 0 = Just (attenuation, scattered, g1)
        | otherwise = Nothing
        where
            reflected = reflect (direction rIn) (normal hr)
            (ruVec, g1) = randomUnitVec g
            reflectedFuzzy = unitVector reflected + scale ruVec (fuzz metal)
            scattered = Ray (p hr) reflectedFuzzy
            attenuation = albedoM metal

-- Define Lambertian as a newtype
newtype Lambertian = Lambertian {
    albedoL :: Color
}

-- Define lambertian's scatter function
instance Material Lambertian where
    scatter g _ hr lamb
        | nearZero scatterDir = Just (attenuation, scatteredNz, g')
        | otherwise = Just (attenuation, scattered, g')
        where
            (ruVec, g') = randomUnitVec g
            scatterDir = normal hr + ruVec
            scattered = Ray (p hr) scatterDir
            scatteredNz = Ray (p hr) (normal hr)
            attenuation = albedoL lamb
