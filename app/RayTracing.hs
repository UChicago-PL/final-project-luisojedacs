module Main where
import System.IO (hPutStr, hFlush)
import GHC.IO.StdHandles (stderr)
import Control.Monad (forM_)
import Color (colorString)
import Ray (Ray(..))
import Vec3 (Vec3(..), Point3, scaleDown, scale)
import Utils (rayColor)
import HittableList (HittableList(..))
import Sphere (Sphere(..))

aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
imageWidth = 400

rawImageHeight :: Int
rawImageHeight = truncate (fromIntegral imageWidth / aspectRatio)

imageHeight :: Int
imageHeight = max rawImageHeight 1

focalLength :: Double
focalLength = 1.0

viewportHeight :: Double
viewportHeight = 2.0

viewportWidth :: Double
viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight);

cameraCenter :: Point3
cameraCenter = Vec3 0 0 0

viewportU :: Vec3
viewportU = Vec3 viewportWidth 0 0

viewportV :: Vec3
viewportV = Vec3 0 (-viewportHeight) 0

pixelDeltaU :: Vec3
pixelDeltaU = scaleDown viewportU (fromIntegral imageWidth)

pixelDeltaV :: Vec3
pixelDeltaV = scaleDown viewportV (fromIntegral imageHeight)

viewportUpperLeft :: Point3
viewportUpperLeft =  cameraCenter - Vec3 0 0 focalLength
                                  - scaleDown viewportU 2
                                  - scaleDown viewportV 2

pixel00Loc :: Point3
pixel00Loc = viewportUpperLeft + scale (pixelDeltaU + pixelDeltaV) 0.5

currWorld :: HittableList
currWorld = HittableList [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5)(-1)) 100]

main :: IO () -- PPM printing etc done in main since it's stateful, error logging, etc.
main = do
  let w = imageWidth :: Int -- for convenience
      h = imageHeight :: Int
  putStr $ "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" -- Print header
  forM_ [0 .. h - 1] $ \j -> do--Switching to monadic loop for logging (like flipped mapM_)--throw away side effects
    hPutStr stderr $ "\rScanlines remaining " ++ show (h - j) ++ " " -- hPutStr bc newline would make \r pointless
    hFlush stderr -- force stderr to be shown as progress occurs
    forM_ [0 .. w - 1] $ \i -> do
      let pixelCenter = pixel00Loc + scale pixelDeltaU (fromIntegral i)
                                   + scale pixelDeltaV (fromIntegral j)
          rayDirection = pixelCenter - cameraCenter -- not unit--faster code
          r = Ray cameraCenter rayDirection
          pixelColor = rayColor r currWorld
      putStrLn $ colorString pixelColor
  hPutStr stderr "\rDone.                       \n"
