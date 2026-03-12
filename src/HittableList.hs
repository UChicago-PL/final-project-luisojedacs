module HittableList where
-- The module defining HittableList --

import Hittable
import Sphere ( Sphere )
import Data.Foldable (foldl')
import Interval (Interval(..))
import Utils ( HitRecord(t) )
import Control.Applicative ((<|>))

-- The HittableList type creates a container for "hittables" as defined in Hittable.hs.
-- For the milestone I reached, I simply made it a list of Spheres.
newtype HittableList = HittableList [Sphere]

-- Making HittableList an instance of the Hittable class
instance Hittable HittableList where
    -- Find the closest hit from the ray among all the Hittables in the HittableList
    hit (HittableList items) r intval =
        foldl' closestHit Nothing items
        where
            -- followed hint from hlint
            closestHit acc obj = hit obj r (Interval (bMin intval) currentMax) <|> acc
                where currentMax = maybe (bMax intval) t acc