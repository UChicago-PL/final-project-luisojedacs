module Main where

import qualified MyLib (someFunc)
import qualified Data.Vector.Unboxed as VU --Use unboxed array for better performance

doub2Color :: Double -> Int
doub2Color d = floor (255.999 * d) 

computeBasicColor :: Int -> Int -> Int -> Int -> (Int, Int, Int)
computeBasicColor x y h w =
  let r = doub2Color ((fromIntegral x :: Double) / fromIntegral (w - 1))
      g = doub2Color ((fromIntegral y :: Double) / fromIntegral (h - 1))
      b = 0
  in (r, g, b)

basicImage :: Int -> Int -> VU.Vector (Int, Int, Int)
basicImage h w = VU.generate (h * w) $ \i ->
  let x = i `mod` w
      y = i `div` w
  in computeBasicColor x y h w

pixelToString :: (Int, Int, Int) -> String
pixelToString (a, b, c) =
  show a ++ " " ++
  show b ++ " " ++
  show c

pixVecToString :: Int -> Int -> VU.Vector (Int, Int, Int) -> String
pixVecToString h w vec = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" ++
  unlines (map pixelToString (VU.toList vec))

main :: IO () -- PPM printing etc done in main since it's stateful, error logging, etc.
main = do
  let w = 256 :: Int -- for convenience
      h = 256 :: Int
  putStrLn $ "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" -- Print header
  mapM_ putStrLn --Map monadic function over list, throw away result, keep only side effects 
    [ show a ++ " " ++ show b ++ " " ++ show c

    ]
  
  --putStr (pixVecToString 256 256 (basicImage 256 256))
