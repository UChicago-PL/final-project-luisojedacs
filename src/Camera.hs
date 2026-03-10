module Camera where
import Ray
import HittableList
import Color
import Hittable (Hittable(..))
import Interval (Interval(..), infinity)
import Utils (alpha, randomDouble, SomeMaterial (..), HitRecord (..), Material (..))
import Vec3 (Vec3(..), scale, Point3, scaleDown)
import Control.Monad (forM_)
import GHC.IO.Handle (hPutStr, hFlush)
import GHC.IO.Handle.FD (stderr)
import System.Random (StdGen, getStdGen)
import Data.Foldable (foldl')
import Data.List (mapAccumL)
import Data.IORef (readIORef, newIORef, writeIORef)

data Camera = Camera {
    aspectRatio   :: Double,
    imageWidth    :: Int,
    imageHeight   :: Int,
    center        :: Point3,
    pixelDeltaU   :: Vec3,
    pixelDeltaV   :: Vec3,
    pixel00Loc    :: Point3,
    samplesPerPixel :: Int,
    pixelSamplesScale :: Double,
    maxDepth :: Int
}

rayColor :: StdGen -> Ray -> Int -> HittableList -> (Color, StdGen)
rayColor g _ 0 _ = (Vec3 0 0 0, g)
rayColor g r maxD world =
      case hit world r (Interval 0.001 infinity) of
            Just hr -> case material hr of --if there is a material
                SomeMaterial mat -> case scatter g r hr mat of
                    Just (attenuation, scattered, g1) ->
                        let (bounceColor, g2) = rayColor g1 scattered (maxD - 1) world
                        in (attenuation * bounceColor, g2)
                    Nothing -> (Vec3 0 0 0, g)
            Nothing -> 
                (scale (Vec3 1.0 1.0 1.0) (1 - alpha r) + scale (Vec3 0.5 0.7 1.0) (alpha r), g)

initializeCamera :: Double -> Int -> Int -> Camera
initializeCamera ar iw samps = Camera
    {
        aspectRatio   = ar,
        imageWidth    = iw,
        imageHeight   = ih,
        center        = c,
        pixelDeltaU   = pdu,
        pixelDeltaV   = pdv,
        pixel00Loc    = viewportUpperLeft + scale (pdu + pdv) 0.5,
        samplesPerPixel = samps,
        pixelSamplesScale = 1.0 / fromIntegral samps,
        maxDepth = maxD
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
        maxD = 50

sampleSquare :: StdGen -> (Vec3, StdGen)
sampleSquare g = (Vec3 (x - 0.5) (y - 0.5) 0, g2)
    where
        (x, g1) = randomDouble g
        (y, g2) = randomDouble g1

getRay :: StdGen -> Camera -> Int -> Int -> (Ray, StdGen)
getRay g cam i j = (Ray rayOrigin rayDirection, gNew)
    where
        (offset, gNew) = sampleSquare g
        p00  = pixel00Loc cam
        pdU  = pixelDeltaU cam
        pdV  = pixelDeltaV cam
        pixelSample  = p00 + scale pdU (fromIntegral i + e0 offset) + scale pdV (fromIntegral j + e1 offset)
        rayOrigin = center cam
        rayDirection = pixelSample - rayOrigin

render :: Camera -> HittableList -> IO ()
render cam world = do
  putStr $ "P3\n" ++ show iw ++ " " ++ show ih ++ "\n255\n" -- Print header
  -- need randomness because we are sampling what color the pixel's area would be based on
  --    slight ray offsets; we average this (using the scaling with pixelSamplesScale) resulting
  --    in smoother edges for the sphere
  gRef <- newIORef =<< getStdGen
  forM_ [0 .. ih - 1] $ \j -> do--Switching to monadic loop for logging (like flipped mapM_)--throw away side effects
    hPutStr stderr $ "\rScanlines remaining " ++ show (ih - j) ++ " " -- hPutStr bc newline would make \r pointless
    hFlush stderr -- force stderr to be shown as progress occurs
    forM_ [0 .. iw - 1] $ \i -> do
        g <- readIORef gRef
        --like fold and map combined--collect outputs while still passing g'
        let (gFinal, colors) = mapAccumL (\g' _ -> 
                let (r, gNew) = getRay g' cam i j 
                in (gNew, rayColor gNew r maxD world)
                ) 
                g [0 .. sampPerPixel - 1]
        writeIORef gRef gFinal
        --"accumulate" to zero-vec here--TODO maybe add zeroVec constant
        -- also, scale color by pixelSamplesScale, keeping within valid range (would clamp to white otherwise)
        putStrLn $ colorString (scale (foldl' (+) (Vec3 0 0 0) (map fst colors)) $ pixelSamplesScale cam)
  hPutStr stderr "\rDone.                       \n"
  where
    iw   = imageWidth cam
    ih   = imageHeight cam
    sampPerPixel = samplesPerPixel cam
    maxD = maxDepth cam
