module Main where
-- Module to create scenes, put everything together --

import Vec3 (Vec3(..))
import HittableList (HittableList(..))
import Material
import Sphere (Sphere(..))
import Camera (Camera(..), initializeCamera, render)
import Utils (SomeMaterial(..))
import System.Environment (getArgs)
import GHC.IO.Handle (hPutStr)
import System.Exit (exitFailure)
import GHC.IO.Handle.FD (stderr)

-- Scene: camera + world pair
data Scene = Scene Camera HittableList

-- Parse, say "--scene <name>" from args, default to "demo" if no args
parseArgs :: [String] -> Maybe String
parseArgs ["--scene", name] = Just name
parseArgs _                 = Nothing

lookupScene :: String -> Maybe Scene
lookupScene "demo"       = Just demoScene
lookupScene "glass-demo" = Just glassScene
lookupScene "metals"     = Just metalsScene
lookupScene _            = Nothing

-- Scene definitions --
demoScene :: Scene
demoScene = Scene cam world
  where
    cam = initializeCamera (16.0 / 9.0) 400 100
    world = HittableList
      [ 
        Sphere (Vec3 0 (-100.5) (-1)) 100    (SomeMaterial (Lambertian (Vec3 0.8 0.8 0.0))),
        Sphere (Vec3 0 0 (-1.2))      0.5    (SomeMaterial (Lambertian (Vec3 0.1 0.2 0.5))),
        Sphere (Vec3 (-1) 0 (-1))     0.5    (SomeMaterial (Dielectric 1.5)),
        Sphere (Vec3 (-1) 0 (-1))     0.4    (SomeMaterial (Dielectric (1 / 1.50))),
        Sphere (Vec3 1 0 (-1))        0.5    (SomeMaterial (Metal (Vec3 0.8 0.6 0.2) 1))
      ]

glassScene :: Scene
glassScene = Scene cam world
  where
    cam = initializeCamera (16.0 / 9.0) 400 100
    world = HittableList
      [ 
        Sphere (Vec3 0 (-100.5) (-1)) 100    (SomeMaterial (Lambertian (Vec3 0.2 0.8 0.2))),
        Sphere (Vec3 0 0 (-1))        0.5    (SomeMaterial (Dielectric 1.5)),
        Sphere (Vec3 0 0 (-1))        0.4    (SomeMaterial (Dielectric (1 / 1.5))),
        Sphere (Vec3 (-1.2) 0 (-1))   0.5    (SomeMaterial (Dielectric 2.4)),
        Sphere (Vec3 1.2 0 (-1))      0.5    (SomeMaterial (Dielectric 1.33))
      ]

metalsScene :: Scene
metalsScene = Scene cam world
  where
    cam = initializeCamera (16.0 / 9.0) 400 100
    world = HittableList
      [ 
        Sphere (Vec3 0 (-100.5) (-1)) 100    (SomeMaterial (Lambertian (Vec3 0.5 0.5 0.5))),
        Sphere (Vec3 0 0 (-1))        0.5    (SomeMaterial (Metal (Vec3 0.8 0.8 0.8) 0)),
        Sphere (Vec3 (-1.2) 0 (-1))   0.5    (SomeMaterial (Metal (Vec3 0.8 0.4 0.2) 0.3)),
        Sphere (Vec3 1.2 0 (-1))      0.5    (SomeMaterial (Metal (Vec3 0.2 0.4 0.8) 0.8))
      ]


-- Define materials for different spheres --
materialGround :: SomeMaterial
materialGround = SomeMaterial (Lambertian (Vec3 0.8 0.8 0.0))

materialCenter:: SomeMaterial
materialCenter = SomeMaterial (Lambertian (Vec3 0.1 0.2 0.5))

materialLeft :: SomeMaterial
materialLeft = SomeMaterial (Dielectric 1.5)

materialBubble :: SomeMaterial
materialBubble = SomeMaterial (Dielectric (1 / 1.50))

materialRight :: SomeMaterial
materialRight = SomeMaterial (Metal (Vec3 0.8 0.6 0.2) 1)

-- Define the world --
currWorld :: HittableList
currWorld = HittableList 
  [
    Sphere (Vec3 0 (-100.5)(-1)) 100 materialGround,
    Sphere (Vec3 0 0 (-1.2)) 0.5 materialCenter, 
    Sphere (Vec3 (-1) 0 (-1)) 0.5 materialLeft,
    Sphere (Vec3 (-1) 0 (-1)) 0.4 materialBubble,
    Sphere (Vec3 1 0 (-1)) 0.5  materialRight
  ]

-- Initialize the camera --
currCamera :: Camera
currCamera = initializeCamera (16.0 / 9.0) 1600 400

-- Main function --
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Nothing -> do
        hPutStr stderr "usage: RayTracing [--scene <name>]\n"
        exitFailure
      Just name -> case lookupScene name of
        Nothing -> do
          hPutStr stderr $ "unknown scene: " ++ name ++ "\n"
          hPutStr stderr "available scenes: demo, glass-demo, metals\n"
          exitFailure
        Just (Scene cam world) -> render cam world