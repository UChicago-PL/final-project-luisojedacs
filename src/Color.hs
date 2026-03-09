module Color where
import Vec3 (Vec3(..))
import Interval (Interval(..), clamp)

-- Module for the Color type/associated fns
type Color = Vec3
doub2Color :: Double -> Int
doub2Color d = truncate (256 * d)



colorString :: Color -> String --implement this way for now
colorString (Vec3 r g b) =  show (doub2Color $ clamp r intensity) ++ " " ++
                            show (doub2Color $ clamp g intensity) ++ " " ++
                            show (doub2Color $ clamp b intensity) ++ "\n"
        where
            intensity = Interval 0 0.999
