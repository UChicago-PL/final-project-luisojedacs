module Camera where
import Ray
import HittableList
import Color
import Hittable (Hittable(..), HitRecord (..))
import Interval (Interval(..), infinity)
import Utils (alpha)
import Vec3 (Vec3(..), scale, Point3, scaleDown)
import Control.Monad (forM_)
import GHC.IO.Handle (hPutStr, hFlush)
import GHC.IO.Handle.FD (stderr)

data Camera = Camera {
    aspectRatio   :: Double,
    imageWidth    :: Int,
    imageHeight   :: Int,
    center        :: Point3,
    pixelDeltaU   :: Vec3,
    pixelDeltaV   :: Vec3,
    pixel00Loc    :: Point3
}

rayColor :: Ray -> HittableList -> Color
rayColor r world =
      case hit world r (Interval 0 infinity) of
            Just rec -> scale (normal rec + Vec3 1 1 1) 0.5
            Nothing -> scale (Vec3 1.0 1.0 1.0) (1 - alpha r) + scale (Vec3 0.5 0.7 1.0) (alpha r)

initializeCamera :: Double -> Int -> Camera
initializeCamera ar iw = Camera
    { aspectRatio   = ar
    , imageWidth    = iw
    , imageHeight   = ih
    , center        = c
    , pixelDeltaU   = pdu
    , pixelDeltaV   = pdv
    , pixel00Loc    = viewportUpperLeft + scale (pdu + pdv) 0.5
    }
    where
        ih = max (truncate (fromIntegral iw / ar)) 1
        c  = Vec3 0 0 0
        focalLength = 1.0
        viewportHeight = 2.0
        viewportWidth = viewportHeight * (fromIntegral iw / fromIntegral ih)
        viewportU = Vec3 viewportWidth 0 0
        viewportV = Vec3 0 (-viewportHeight) 0
        pdu = scaleDown viewportU (fromIntegral iw)
        pdv = scaleDown viewportV (fromIntegral ih)
        viewportUpperLeft = c 
                            - Vec3 0 0 focalLength
                            - scaleDown viewportU 2
                            - scaleDown viewportV 2

render :: Camera -> HittableList -> IO ()
render cam world = do
  putStr $ "P3\n" ++ show iw ++ " " ++ show ih ++ "\n255\n" -- Print header
  forM_ [0 .. ih - 1] $ \j -> do--Switching to monadic loop for logging (like flipped mapM_)--throw away side effects
    hPutStr stderr $ "\rScanlines remaining " ++ show (ih - j) ++ " " -- hPutStr bc newline would make \r pointless
    hFlush stderr -- force stderr to be shown as progress occurs
    forM_ [0 .. iw - 1] $ \i -> do
      let pixelCenter = p00 + scale pdU (fromIntegral i)
                                   + scale pdV (fromIntegral j)
          rayDirection = pixelCenter - cameraCenter -- not unit--faster code
          r = Ray cameraCenter rayDirection
          pixelColor = rayColor r world
      putStrLn $ colorString pixelColor
  hPutStr stderr "\rDone.                       \n"
  where
    iw   = imageWidth cam
    ih   = imageHeight cam
    p00 = pixel00Loc cam
    pdU  = pixelDeltaU cam
    pdV  = pixelDeltaV cam
    cameraCenter  = center cam
