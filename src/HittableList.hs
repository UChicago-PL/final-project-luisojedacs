{- HLINT ignore "Use newtype instead of data" -}
module HittableList where
import Hittable
import Sphere ( Sphere )
import Data.Foldable (foldl')
import Interval (Interval(..))
import Utils ( HitRecord(t) )
import Control.Applicative ((<|>))

newtype HittableList = HittableList [Sphere]

instance Hittable HittableList where
    hit (HittableList items) r intval =
        foldl' closestHit Nothing items
        where
            -- followed hint from hlint
            closestHit acc obj = hit obj r (Interval (bMin intval) currentMax) <|> acc
                where currentMax = maybe (bMax intval) t acc