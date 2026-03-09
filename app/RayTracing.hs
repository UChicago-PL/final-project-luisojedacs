module Main where
import Vec3 (Vec3(..))
import HittableList (HittableList(..))
import Sphere (Sphere(..))
import Camera (Camera(..), initializeCamera, render)

currWorld :: HittableList
currWorld = HittableList [Sphere (Vec3 0 0 (-1)) 0.5, Sphere (Vec3 0 (-100.5)(-1)) 100]

currCamera :: Camera
currCamera = initializeCamera (16.0 / 9.0) 400 100

main :: IO () -- PPM printing etc done in main since it's stateful, error logging, etc.
main = render currCamera currWorld
