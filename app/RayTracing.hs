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
import Camera (Camera(..), initializeCamera, render)

currWorld :: HittableList
currWorld = HittableList [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5)(-1)) 100]

currCamera :: Camera
currCamera = initializeCamera (16.0 / 9.0) 400

main :: IO () -- PPM printing etc done in main since it's stateful, error logging, etc.
main = render currCamera currWorld
