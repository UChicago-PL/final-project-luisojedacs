module Color where
import Vec3 (Vec3(..))
import Interval (Interval(..), clamp)

-- Module for the Color type/associated fns
type Color = Vec3
doub2Color :: Double -> Int
doub2Color d = truncate (256 * d)

linear2Gamma :: Double -> Double
linear2Gamma linearComponent
    | linearComponent > 0 = sqrt linearComponent
    | otherwise = 0

colorString :: Color -> String --implement this way for now
colorString (Vec3 r g b) =  show (doub2Color $ clamp lr intensity) ++ " " ++
                            show (doub2Color $ clamp lg intensity) ++ " " ++
                            show (doub2Color $ clamp lb intensity) ++ "\n"
        where
            intensity = Interval 0 0.999
            (lr, lg, lb) = (linear2Gamma r, linear2Gamma g, linear2Gamma b)
