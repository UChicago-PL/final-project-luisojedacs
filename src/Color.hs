module Color where
import Vec3 (Vec3(..))

-- Module for the Color type/associated fns
type Color = Vec3
doub2Color :: Double -> Int
doub2Color d = truncate (255.999 * d)



colorString :: Color -> String --implement this way for now
colorString (Vec3 r g b) =  show (doub2Color r) ++ " " ++
                            show (doub2Color g) ++ " " ++
                            show (doub2Color b) ++ "\n"
