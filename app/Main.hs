module Main where

doub2Color :: Double -> Int
doub2Color d = floor (255.999 * d) 

main :: IO () -- PPM printing etc done in main since it's stateful, error logging, etc.
main = do
  let w = 256 :: Int -- for convenience
      h = 256 :: Int
  putStrLn $ "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" -- Print header
  mapM_ putStrLn --Map monadic function over list, throw away result, keep only side effects 
    [ show r ++ " " ++ show g ++ " " ++ show b
    | y <- [0 .. h - 1]
    , x <- [0 .. w - 1] -- comprehension goes in order of this list first, so "iterating by rows"
    , let r = doub2Color ((fromIntegral x :: Double) / fromIntegral (w - 1))
          g = doub2Color ((fromIntegral y :: Double) / fromIntegral (h - 1))
          b = 0 :: Int
    ]

